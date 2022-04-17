package main

import (
	"fmt"
)

func main() {
	fmt.Printf("Hello, new project!\n")
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}
