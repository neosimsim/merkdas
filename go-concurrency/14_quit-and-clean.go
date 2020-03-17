// Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
// Use of this source code is governed by a BSD 3-clause
// style license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"math/rand"
	"time"
)

func cleanup() {
}

func boring(msg string, quit chan string) <-chan string {
	c := make(chan string)
	go func() {
		for i := 0; ; i++ {
			select {
			case c <- fmt.Sprintf("%s %d", msg, i):
				// do nothing
			case <-quit:
				cleanup()
				quit <- "See you!"
				return
			}
			time.Sleep(time.Duration(rand.Intn(1e3)) * time.Millisecond)
		}
	}()
	return c // Returnthe cannel to the caller
}

func main() {
	quit := make(chan string)
	c := boring("Joe", quit)
	for i := rand.Intn(10); i >= 0; i-- {
			fmt.Println(<-c)
	}
	quit <- "Bye!"
	fmt.Printf("Joe says: %q\n", <-quit)
}
