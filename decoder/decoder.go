package decoder

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"
	"strings"
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

type Function struct {
	Name   string
	Ups    int
	Params int
	IsVar  int
	Stack  int
	Ops    []uint32
	Const  []interface{}
	Protos []*Function

	Parent *Function
}

func (f *Function) DebugPrint(level int) {
	pad := strings.Repeat(" ", level * 4)

	fmt.Printf("%s-- %#v %d ups, %d params, %d var, %d stack, %d ops, %d const, %d protos\n", pad, f.Name, f.Ups, f.Params, f.IsVar, f.Stack, len(f.Ops), len(f.Const), len(f.Protos))
	for k, v := range f.Const {
		fmt.Printf("%s%04X: %#v\n", pad, k, v)
	}
	for k, v := range f.Ops {
		fmt.Printf("%s%04X: %08X\n", pad, k, v)
	}
	for _, v := range f.Protos {
		v.DebugPrint(level + 1)
	}
}

func Decode(data []byte) *Function {
	var enc binary.ByteOrder = binary.BigEndian

	if len(data) < 21 + 4 + 8 + 4 + 6 * 4 {
		panic(fmt.Errorf("Bad data"))
	}

	if enc.Uint32(data[0:4]) != 0x1b4c7561 {
		panic(fmt.Errorf("Bad header magic"))
	}

	if enc.Uint32(data[4:8]) != 0x51000104 {
		panic(fmt.Errorf("Bad header version"))
	}

	if enc.Uint32(data[8:12]) != 0x04040800 {
		panic(fmt.Errorf("Unsupported data types"))
	}

	data = data[12:]

	enc = binary.LittleEndian

	byteInt := func() int {
		value := data[0]
		data = data[1:]
		return int(value)
	}

	integer := func() int {
		value := enc.Uint32(data[0:4])
		data = data[4:]
		return int(value)
	}

	string := func() string {
		l := integer()
		if l == 0 {
			return ""
		}
		strData := data[:l]
		data = data[l:]
		end := bytes.IndexByte(strData, 0)
		return string(strData[:end])
	}

	number := func() float64 {
		value := math.Float64frombits(enc.Uint64(data[0:8]))
		data = data[8:]
		return value
	}

	konst := func() interface{} {
		t := byteInt()
		switch t {
		case 0:
			return nil
		case 1:
			return byteInt() != 0
		case 3:
			return number()
		case 4:
			return string()
		default:
			panic(t)
		}
	}

	var decodeFunction func() *Function
	decodeFunction = func() *Function {
		f := &Function{}
		f.Name = string()
		integer()
		integer()
		f.Ups = byteInt()
		f.Params = byteInt()
		f.IsVar = byteInt()
		f.Stack = byteInt()

		f.Ops = make([]uint32, integer())
		for k := range f.Ops {
			f.Ops[k] = uint32(integer())
		}

		f.Const = make([]interface{}, integer())
		for k := range f.Const {
			f.Const[k] = konst()
		}

		f.Protos = make([]*Function, integer())
		for k := range f.Protos {
			f.Protos[k] = decodeFunction()
			f.Protos[k].Parent = f
		}

		// Skip lines
		data = data[integer() * 4:]

		// Skip locals
		locals := integer()
		for i := 0; i < locals; i++ {
			string()
			integer()
			integer()
		}

		// Skip upvalues
		upvalues := integer()
		for i := 0; i < upvalues; i++ {
			string()
		}

		return f
	}

	return decodeFunction()
}

