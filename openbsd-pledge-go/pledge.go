// +build openbsd

package main

import (
	"golang.org/x/sys/unix"
	"log"
)

func init() {
	if err := unix.Pledge("stdio", nil); err != nil {
		log.Panic("Error on pledge: ", err)
	}
}
