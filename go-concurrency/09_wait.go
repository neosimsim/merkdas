// Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
// Use of this source code is governed by a BSD 3-clause
// style license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Message struct {
	str string
	wait chan bool
}

func boring(msg string) <-chan Message {
	c := make(chan Message)
	waitForIt := make(chan bool) // Shared between all messages.
	go func() {
		for i := 0; ; i++ {
			c <- Message{ fmt.Sprintf("%s %d", msg, i), waitForIt }
			time.Sleep(time.Duration(rand.Intn(1e3)) * time.Millisecond)
			<-waitForIt
		}
	}()
	return c
}

// We use a fan-in function to let whosoever is ready talk.
func fanIn(input1, input2 <-chan Message) <-chan Message {
	c := make(chan Message)
	go func() { for { c <- <-input1 } }()
	go func() { for { c <- <-input2 } }()
	return c
}

func main() {
	c := fanIn(boring("Joe"), boring("Ann"))
	for i := 0; i < 5; i++ {
		msg1 := <- c; fmt.Println(msg1.str)
		msg2 := <- c; fmt.Println(msg2.str)
		msg1.wait <- true
		msg2.wait <- true
	}
	fmt.Println("You're boring; I'm leaving.")
}
