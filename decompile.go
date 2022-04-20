package main

import (
	"fmt"
	"strings"

	"github.com/dexter3k/deluac/decoder"
)

const (
	LFIELDS_PER_FLUSH = 50
)

func MakePadding(level int) string {
	return strings.Repeat(" ", 4 * level)
}



type UnaryOp int
const (
	UnaryNot UnaryOp = iota
	UnaryNegate
	UnaryLength
)

func (u UnaryOp) String() string {
	if u > UnaryLength {
		return fmt.Sprintf("UnaryOp(%d)", int(u))
	}
	return []string{
		"not", "-", "#",
	}[u]
}

type BinaryOp int
const (
	BinaryLess BinaryOp = iota
	BinaryGreater
	BinaryLessEqual
	BinaryGreaterEqual
	BinaryNotEqual
	BinaryEqual
	BinaryAdd
	BinarySub
	BinaryMul
	BinaryDiv
	BinaryMod
	BinaryPow
	BinaryConcat
)

func (c BinaryOp) String() string {
	if c > BinaryConcat {
		return fmt.Sprintf("BinaryOp(%d)", int(c))
	}
	return []string{
		"<", ">", "<=", ">=", "~=", "==", "+", "-", "*", "/", "%", "^", "..",
	}[c]
}



type Statement interface {
	Print(level int) string
}

type Expression interface {
	String() string
}

func FormatExprList(args []Expression, addSpace bool) string {
	if len(args) == 0 {
		return ""
	}

	ret := ""
	if addSpace {
		ret += " "
	}
	for i := 0; i < len(args); i++ {
		ret += args[i].String()
		if i != len(args) - 1 {
			ret += ", "
		}
	}

	return ret
}



type GlobalRef string

func (gr GlobalRef) String() string {
	return string(gr)
}

type VarargRef struct{}

func (vr VarargRef) String() string {
	return "..."
}

type RegRef int

func (rr RegRef) String() string {
	return fmt.Sprintf("r%d", int(rr))
}

type UpvalRef int

func (ur UpvalRef) String() string {
	return fmt.Sprintf("u%d", int(ur))
}

type RestRegRef int

func (rrr RestRegRef) String() string {
	return fmt.Sprintf("r%d...", int(rrr))
}

type NilExpression struct{}

func (ne NilExpression) String() string {
	return "nil"
}

type NumberExpression float64

func (ne NumberExpression) String() string {
	return fmt.Sprintf("%#v", float64(ne))
}

type StringExpression string

func (se StringExpression) String() string {
	return fmt.Sprintf("%#v", string(se))
}

type BoolExpression bool

func (be BoolExpression) String() string {
	return fmt.Sprintf("%#v", bool(be))
}

type NewTableExpression struct{
}

func (nte NewTableExpression) String() string {
	return "{}"
}

type RefTableExpression struct{
	Table Expression
	Index Expression
}

func (rte RefTableExpression) String() string {
	return rte.Table.String() + "[" + rte.Index.String() + "]"
}

type CallExpression struct {
	Func Expression
	Args []Expression
}

func (ce CallExpression) String() string {
	return ce.Func.String() + "(" + FormatExprList(ce.Args, false) + ")"
}

type ClosureExpression struct {
	F   *decoder.Function
	Ups []Expression
}

func (ce ClosureExpression) String() string {
	return "function(" + FormatExprList(ce.Ups, false) + ")(todo) todo end"
}

type UnaryExpression struct {
	Op UnaryOp
	E  Expression
}

func (ue UnaryExpression) String() string {
	return fmt.Sprintf("%s (%s)", ue.Op, ue.E)
}

type BinaryExpression struct {
	Op    BinaryOp
	Left  Expression
	Right Expression
}

func (be BinaryExpression) String() string {
	return fmt.Sprintf("(%s) %s (%s)", be.Left, be.Op, be.Right)
}



// RETURN R(A), ..., R(A+B-2)
// B 2+ => return B-1 args starting from A
// B 1  => return no args
// B 0  => return unknown (must have unknown/varargs at top of stack)
type ReturnStatement struct {
	BaseReg int
	Count   int
	Unknown bool
	Args    []Expression
}

func (rs ReturnStatement) Print(level int) string {
	ret := MakePadding(level) + "return" + FormatExprList(rs.Args, true)
	return ret + "\n"
}

type AssignStatement struct {
	Left  []Expression
	Right []Expression
}

func (as AssignStatement) Print(level int) string {
	return MakePadding(level) + FormatExprList(as.Left, false) + " = " + FormatExprList(as.Right, false) + "\n"
}

type CallStatement struct {
	Call *CallExpression
}

func (cs CallStatement) Print(level int) string {
	return MakePadding(level) + cs.Call.String() + "\n"
}

type JumpStatement struct {
	Target int
}

func (js JumpStatement) Print(level int) string {
	return fmt.Sprintf("%sjmp %d\n", MakePadding(level), js.Target)
}

type IfStatement struct {
	Cond   Expression
	Target int
}

func (is IfStatement) Print(level int) string {
	return fmt.Sprintf("%sif (%s) then jmp %d end\n", MakePadding(level), is.Cond, is.Target)
}



func Decompile(f *decoder.Function) {
	loadK := func(idx int) Expression {
		switch v := f.Const[idx].(type) {
		case float64:
			return NumberExpression(v)
		case string:
			return StringExpression(v)
		case bool:
			return BoolExpression(v)
		case nil:
			return NilExpression{}
		default:
			panic(fmt.Errorf("Unknown value: %T %#v", v, v))
		}
	}

	rk := func(idx int) Expression {
		if idx < 256 {
			return RegRef(idx)
		}
		return loadK(idx - 256)
	}

	findGlobal := func(idx int) GlobalRef {
		switch v := f.Const[idx].(type) {
		case string:
			return GlobalRef(v)
		default:
			panic("Global is not refd by a string")
		}
	}

	jTargets := map[int]int{}
	jumps := []int{}

	code := make([]Statement, len(f.Ops))
	for i := 0; i < len(code); i++ {
		var op decoder.Opcode
		decoder.DecodeOp(f.Ops[i], &op)

		switch op.Op {
		case decoder.MOVE:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{RegRef(op.B)},
			}
		case decoder.LOADK:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{loadK(op.Bx)},
			}
		case decoder.LOADBOOL:
			if op.C != 0 {
				panic("TODO")
			}
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{BoolExpression(op.B != 0)},
			}
		case decoder.LOADNIL:
			l := make([]Expression, op.B - op.A + 1)
			r := make([]Expression, op.B - op.A + 1)
			for j := 0; j < len(l); j++ {
				l[j] = RegRef(op.A + j)
				r[j] = NilExpression{}
			}
			code[i] = &AssignStatement{
				Left:  l,
				Right: r,
			}
		case decoder.GETUPVAL:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{UpvalRef(op.B)},
			}
		case decoder.GETGLOBAL:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{findGlobal(op.Bx)},
			}
		case decoder.GETTABLE:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&RefTableExpression{Table: RegRef(op.B), Index: rk(op.C)}},
			}
		case decoder.SETGLOBAL:
			code[i] = &AssignStatement{
				Left:  []Expression{findGlobal(op.Bx)},
				Right: []Expression{RegRef(op.A)},
			}
		case decoder.SETUPVAL:
			code[i] = &AssignStatement{
				Left:  []Expression{UpvalRef(op.B)},
				Right: []Expression{RegRef(op.A)},
			}
		case decoder.SETTABLE:
			code[i] = &AssignStatement{
				Left:  []Expression{&RefTableExpression{Table: RegRef(op.A), Index: rk(op.B)}},
				Right: []Expression{rk(op.C)},
			}
		case decoder.NEWTABLE:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&NewTableExpression{}},
			}
		case decoder.SELF:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A + 1), RegRef(op.A)},
				Right: []Expression{RegRef(op.B), &RefTableExpression{Table: RegRef(op.B), Index: rk(op.C)}},
			}
		case decoder.ADD:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinaryAdd}},
			}
		case decoder.SUB:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinarySub}},
			}
		case decoder.MUL:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinaryMul}},
			}
		case decoder.DIV:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinaryDiv}},
			}
		case decoder.MOD:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinaryMod}},
			}
		case decoder.POW:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&BinaryExpression{Left: rk(op.B), Right: rk(op.C), Op: BinaryPow}},
			}
		case decoder.UNM:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&UnaryExpression{E: RegRef(op.B), Op: UnaryNegate}},
			}
		case decoder.NOT:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&UnaryExpression{E: RegRef(op.B), Op: UnaryNot}},
			}
		case decoder.LEN:
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{&UnaryExpression{E: RegRef(op.B), Op: UnaryLength}},
			}
		case decoder.CONCAT:
			var tmp Expression = RegRef(op.C)
			for k := op.C - 1; k >= op.B; k-- {
				tmp = &BinaryExpression{
					Left:  RegRef(k),
					Right: tmp,
					Op:    BinaryConcat,
				}
			}
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{tmp},
			}
		case decoder.JMP:
			code[i] = &JumpStatement{
				Target: i + 1 + op.SBx,
			}
			jumps = append(jumps, i)
			jTargets[i + 1 + op.SBx]++
		case decoder.EQ:
			// if (B == C) != A then jmp
			// =>
			// A=0: if (B == C) then jmp
			// A=1: if (B != C) then jmp
			s := &IfStatement{
				Target: i + 1 + 1,
			}
			if op.C == 0 {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryEqual,
				}
			} else {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryNotEqual,
				}
			}
			code[i] = s
			jumps = append(jumps, i)
			jTargets[s.Target]++
		case decoder.LT:
			// if (B < C) != A then jmp
			// =>
			// A=0: if (B < C) then jmp
			// B=0: if (B >= C) then jmp
			s := &IfStatement{
				Target: i + 1 + 1,
			}
			if op.C == 0 {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryLess,
				}
			} else {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryGreaterEqual,
				}
			}
			code[i] = s
			jumps = append(jumps, i)
			jTargets[s.Target]++
		case decoder.LE:
			// if (B <= C) != A then jmp
			// =>
			// A=0: if (B <= C) then jmp
			// B=0: if (B > C) then jmp
			s := &IfStatement{
				Target: i + 1 + 1,
			}
			if op.C == 0 {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryLessEqual,
				}
			} else {
				s.Cond = &BinaryExpression{
					Left:  rk(op.B),
					Right: rk(op.C),
					Op:    BinaryGreater,
				}
			}
			code[i] = s
			jumps = append(jumps, i)
			jTargets[s.Target]++
		case decoder.TEST:
			// if not (R(A) != C) then jmp
			// =>
			// C=0: if R(A) then jmp
			// C=1: if not R(A) then jmp
			s := &IfStatement{
				Target: i + 1 + 1,
			}
			if op.C == 0 {
				s.Cond = RegRef(op.A)
			} else {
				s.Cond = &UnaryExpression{
					Op: UnaryNot,
					E:  RegRef(op.A),
				}
			}
			code[i] = s
			jumps = append(jumps, i)
			jTargets[s.Target]++
		// case decoder.TESTSET:
		case decoder.CALL:
			var args []Expression
			if op.B == 0 {
				args = []Expression{RestRegRef(op.A + 1)}
			} else {
				args = make([]Expression, op.B - 1)
				for j := 0; j < len(args); j++ {
					args[j] = RegRef(op.A + 1 + j)
				}
			}
			callExpr := &CallExpression{
				Func: RegRef(op.A),
				Args: args,
			}

			if op.C == 1 {
				code[i] = &CallStatement{Call: callExpr}
			} else {
				var rets []Expression
				if op.C == 0 {
					rets = []Expression{RestRegRef(op.A)}
				} else {
					rets = make([]Expression, op.C - 1)
					for j := 0; j < len(rets); j++ {
						rets[j] = RegRef(op.A + j)
					}
				}
				code[i] = &AssignStatement{
					Left:  rets,
					Right: []Expression{callExpr},
				}
			}
		case decoder.TAILCALL:
			var args []Expression
			if op.B == 0 {
				args = []Expression{RestRegRef(op.A + 1)}
			} else {
				args = make([]Expression, op.B - 1)
				for j := 0; j < len(args); j++ {
					args[j] = RegRef(op.A + 1 + j)
				}
			}
			callExpr := &CallExpression{
				Func: RegRef(op.A),
				Args: args,
			}
			code[i] = &ReturnStatement{
				BaseReg: op.A,
				Count:   -1,
				Unknown: true,
				Args:    []Expression{callExpr},
			}
		case decoder.RETURN:
			ret := &ReturnStatement{
				BaseReg: op.A,
				Count:   op.B - 1,
				Unknown: op.B == 0,
			}
			if ret.Unknown {
				ret.Args = []Expression{RestRegRef(ret.BaseReg)}
			} else {
				ret.Args = make([]Expression, 0, ret.Count)
				for i := 0; i < ret.Count; i++ {
					ret.Args = append(ret.Args, RegRef(ret.BaseReg + i))
				}
			}
			code[i] = ret
		// case decoder.FORLOOP:
		// case decoder.FORPREP:
		// case decoder.TFORLOOP:
		case decoder.SETLIST:
			if op.C == 0 {
				panic("TODO: find how to even generate this!")
			}
			baseIndex := (op.C - 1) * LFIELDS_PER_FLUSH + 1

			if op.B == 0 {
				// Note: this is not a valid Lua syntax, we must process this!
				// r(A)[baseIndex, ...] = A+1...
				code[i] = &AssignStatement{
					Left:  []Expression{&RefTableExpression{Table: RegRef(op.A), Index: NumberExpression(baseIndex)}},
					Right: []Expression{RestRegRef(op.A + 1)},
				}
			} else {
				l := make([]Expression, op.B)
				r := make([]Expression, op.B)
				for j := 0; j < len(l); j++ {
					l[j] = &RefTableExpression{Table: RegRef(op.A), Index: NumberExpression(baseIndex + j)}
					r[j] = RegRef(op.A + j + 1)
				}
				code[i] = &AssignStatement{
					Left:  l,
					Right: r,
				}
			}
		// case decoder.CLOSE:
		case decoder.CLOSURE:
			proto := f.Protos[op.Bx]
			closure := &ClosureExpression{
				F:   proto,
				Ups: make([]Expression, proto.Ups),
			}
			code[i] = &AssignStatement{
				Left:  []Expression{RegRef(op.A)},
				Right: []Expression{closure},
			}
			for j := 0; j < proto.Ups; j++ {
				i++
				if i == len(code) {
					panic("EOF while matching ups for closure")
				}

				up := f.Op(i)
				switch up.Op {
				case decoder.MOVE:
					closure.Ups[j] = RegRef(up.B)
				case decoder.GETUPVAL:
					closure.Ups[j] = UpvalRef(up.B)
				default:
					panic(fmt.Errorf("Unknown value: %T %#v", up, up))
				}
			}
		case decoder.VARARG:
			if op.B == 0 {
				code[i] = &AssignStatement{
					Left:  []Expression{RestRegRef(op.A)},
					Right: []Expression{VarargRef{}},
				}
			} else {
				left := []Expression{}
				for i := 0; i < op.B; i++ {
					left = append(left, RegRef(op.A + i))
				}
				code[i] = &AssignStatement{
					Left:  left,
					Right: []Expression{VarargRef{}},
				}
			}
		default:
			panic(fmt.Errorf("Unknown op %s", op))
		}
	}

	// Push func args as an easy hack for now...
	pushForwardFuncArgs(code, jTargets)

	level := f.Level()
	for k, v := range code {
		if jTargets[k] != 0 {
			fmt.Printf("%ssub_%d:\n", MakePadding(level), k)
		}
		if v == nil {
			// This code part was left out...
			// fmt.Printf("%s!!!nil or missing!!!\n", MakePadding(level))
			continue
		}
		fmt.Printf("%s", v.Print(level + 1))
	}

	for k, v := range f.Protos {
		fmt.Printf("\n%s-- proto function %d\n", MakePadding(level + 1), k)
		func() {
			defer func() {
				if err := recover(); err != nil {
					fmt.Printf("Failed to decompile: %v\n", err)
				}
			}()

			Decompile(v)
		}()
	}
}

func pushForwardFuncArgs(code []Statement, jTargets map[int]int) {
	for k, v := range code {
		if jTargets[k] != 0 {
			continue
		}

		var call *CallExpression
		switch vv := v.(type) {
		case *CallStatement:
			call = vv.Call
		case *AssignStatement:
			if c, ok := vv.Right[0].(*CallExpression); ok {
				call = c
			}
		}

		if call == nil {
			continue
		}

		base := int(call.Func.(RegRef))

		args := []int{base}
		idxs := []int{}
		for k, v := range call.Args {
			if r, ok := v.(RegRef); ok && int(r) > base {
				args = append(args, int(r))
				idxs = append(idxs, k)
			}
		}

		// Walk back and try to find our args
		for i := k - 1; i >= 0 && len(args) > 0; i-- {
			if code[i] == nil {
				continue
			}

			if ass, ok := code[i].(*AssignStatement); ok {
				if len(ass.Left) != 1 || len(ass.Right) != 1 {
					break
				}

				if lr, ok := ass.Left[0].(RegRef); !ok || int(lr) != args[len(args)-1] {
					break
				}

				args = args[:len(args)-1]
				if len(args) == 0 {
					call.Func = ass.Right[0]
				} else {
					call.Args[idxs[len(idxs)-1]] = ass.Right[0]
					idxs = idxs[:len(idxs)-1]
				}
				code[i] = nil
			} else {
				// Not an assignment, stop now, we're doing something wrong
				break
			}

			if jTargets[k] != 0 {
				// This argument was a jump target, we cannot continue further
				break
			}
		}
	}
}
