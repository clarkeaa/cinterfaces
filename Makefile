all: RamRod.o
RamRod.o: RamRod.c
	gcc -c RamRod.c
RamRod.c: cinterface.lisp cinterface.txt
	./compile.lisp ./cinterface.txt ./RamRod.h ./RamRod.c
clean:
	rm RamRod.h RamRod.c RamRod.o
