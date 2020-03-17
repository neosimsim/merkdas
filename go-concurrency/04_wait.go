// Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
// Use of this source code is governed by a BSD 3-clause
// style license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"time"
	"math/rand"
)

func boring(msg string) {
	for i := 0; ; i++ {
		fmt.Println(msg, i)
		time.Sleep(time.Duration(rand.Intn(1e3)) * time.Millisecond)
	}
}

func main() {
	go boring("boring!")
	fmt.Println("I'm listening")
	time.Sleep(2 * time.Second)
	fmt.Println("You're boring; I'm leaving.")
}
