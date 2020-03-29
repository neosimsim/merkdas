.POSIX:
ALL=pledge
include ../include.mk

#OS=linux
OS=openbsd
OBJ=pledge.o sys_$(OS).o

pledge: $(OBJ)
	$(CC) -o $@ $(OBJ)

clean: c-clean
	rm -f pledge
