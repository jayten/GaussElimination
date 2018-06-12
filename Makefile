all: GE.f90 test.f90
	ifort -r8 -O3 GE.f90 test.f90
