package mta

import (
	"encoding/binary"
	"fmt"
	"math/big"
)

var le = binary.LittleEndian

func Process(data []byte) []byte {
	if len(data) < 0x9d + 5 + 4 {
		panic(fmt.Errorf("Not an mta binary"))
	}

	cipher := data[5:len(data) - 0x9d - 4]
	length := int(le.Uint32(data[len(data) - 0x9d - 4:len(data) - 0x9d]))

	if (len(cipher) % 64) != 0 {
		panic(fmt.Errorf("File has unknown encryption"))
	}

	chunks := len(cipher) / 64

	if length > len(cipher) - chunks {
		panic(fmt.Errorf("length is more than available encrypted data"))
	}

	out := make([]byte, length)
	out_ptr := out[:]

	n := new(big.Int)
	e := new(big.Int)
	c := new(big.Int)
	p := new(big.Int)
	n.SetString("10573226757994254134841319783694652720836391808118779352045062998530656056136257787473735327422320807079659847565541103136825068166499212643083895890948613", 10)
	e.SetString("65537", 10)

	for i := 0; i < chunks; i++ {
		var buf [0x40]byte
		for j := 0; j < 0x40; j++ {
			buf[0x3f - j] = cipher[j]
		}

		c.SetBytes(buf[:])
		p.Exp(c, e, n)

		p.FillBytes(buf[:0x3f])

		for j := 0; j < len(out_ptr) && j < 0x3f; j++ {
			out_ptr[j] = buf[0x3e - j]
		}

		if i != chunks - 1 {
			cipher = cipher[0x40:]
			out_ptr = out_ptr[0x3f:]
		}
	}

	return out
}
