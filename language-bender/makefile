

all: lbend tac2mips tac-interpreter optimips-prime


lbend:
	stack build

tac2mips:
	cd src/tac2mips/
	mkdir bin
	make
	cd ../../

tac-interpreter:
	cd src/tac-interpreter/
	make
	cd ../../

optimips-prime:
	cd src/optimips-prime/
	stack build
	cd ../../

clean:
	cd src/tac2mips/
	make clean
	cd ../../
	cd src/tac-interpreter/
	make clean/
	cd ../../