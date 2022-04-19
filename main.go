package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/dexter3k/deluac/decoder"
	"github.com/dexter3k/deluac/preproc/mta"
)

var preprocessor = flag.String("preproc", "", "set to mta for MTA preprocessor")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		fmt.Printf("Usage: deluac input.luac\n")
		os.Exit(1)
	}

	preproc := strings.ToLower(*preprocessor)
	if preproc != "" && preproc != "mta" {
		fmt.Printf("Unknown preprocessor selected. Available options: mta\n")
		os.Exit(1)
	}

	data := loadBinary(args[0])
	if preproc == "mta" {
		data = mta.Process(data)
	}

	f := decoder.Decode(data)
	f.DebugPrint(0)
	Decompile(f)
}

func loadBinary(path string) []byte {
	f, err := os.Open(path)
	check(err)
	defer f.Close()
	d, err := io.ReadAll(f)
	check(err)
	return d
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}
