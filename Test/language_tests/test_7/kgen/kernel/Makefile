# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := 

ALL_OBJS := kernel_driver.o test7_mod.o kgen_utils.o compute.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 test7_mod.o kgen_utils.o compute.o
	${FC} ${FC_FLAGS} -c -o $@ $<

test7_mod.o: test7_mod.F90 kgen_utils.o compute.o
	${FC} ${FC_FLAGS} -c -o $@ $<

compute.o: compute.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
