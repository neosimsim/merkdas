all: pledge

#OS=linux
OS=openbsd
OBJ=pledge.o sys_$(OS).o

pledge: $(OBJ)
	$(CC) -o $@ $(OBJ)

clean:
	rm -f *.o pledge
