

all: lbend tac2mips tac-interpreter optimips-prime


lbend:
	stack build

tac2mips:
	cd src/tac2mips/ && mkdir -p bin && make

tac-interpreter:
	cd src/tac-interpreter/ && make

optimips-prime:
	cd src/optimips-prime/ && stack build

clean:
	cd src/tac2mips/ && make clean
	cd src/tac-interpreter/ && make clean
