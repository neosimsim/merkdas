package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fmt.Println("Hallo Welt…")
	reader := bufio.NewReader(os.Stdin)
	reader.ReadLine()
}
