package decoder

import (
	"fmt"
)

type OP uint8
const (
	MOVE OP = iota
	LOADK
	LOADBOOL
	LOADNIL
	GETUPVAL
	GETGLOBAL
	GETTABLE
	SETGLOBAL
	SETUPVALUE
	SETTABLE
	NEWTABLE
	SELF
	ADD
	SUB
	MUL
	DIV
	MOD
	POW
	UNM
	NOT
	LEN
	CONCAT
	JMP
	EQ
	LT
	LE
	TEST
	TESTSET
	CALL
	TAILCALL
	RETURN
	FORLOOP
	FORPREP
	TFORLOOP
	SETLIST
	CLOSE
	CLOSURE
	VARARG
)

func (op OP) String() string {
	if op > VARARG {
		return fmt.Sprintf("OP(%d)", int(op))
	}
	return []string{
		"MOVE", "LOADK", "LOADBOOL", "LOADNIL", "GETUPVAL", "GETGLOBAL", "GETTABLE", "SETGLOBAL",
		"SETUPVALUE", "SETTABLE", "NEWTABLE", "SELF", "ADD", "SUB", "MUL", "DIV",
		"MOD", "POW", "UNM", "NOT", "LEN", "CONCAT", "JMP", "EQ",
		"LT", "LE", "TEST", "TESTSET", "CALL", "TAILCALL", "RETURN", "FORLOOP",
		"FORPREP", "TFORLOOP", "SETLIST", "CLOSE", "CLOSURE", "VARARG",
	}[op]
}

type Opcode struct {
	Op OP
	A, B, C, Bx, SBx int
}

func (op Opcode) String() string {
	return fmt.Sprintf("%s A=%d B=%d C=%d Bx=%d sBx=%d", op.Op, op.A, op.B, op.C, op.Bx, op.SBx)
}

func DecodeOp(instr uint32, op_out *Opcode) {
	op_out.Op = OP(instr & 0x3f)
	instr >>= 6
	op_out.A = int(instr & 0xff)
	instr >>= 8
	op_out.Bx = int(instr)
	op_out.SBx = op_out.Bx - 131071
	// TODO: move to this mta preproc
	if op_out.SBx > 0 {
		op_out.SBx = 120000 - op_out.SBx
	}
	op_out.C = int(instr & 0x1ff)
	instr >>= 9
	op_out.B = int(instr)
}
