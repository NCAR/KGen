# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -O3

ALL_OBJS := kernel_driver.o update_mod.o kgen_utils.o calc_mod.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 update_mod.o kgen_utils.o calc_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

update_mod.o: update_mod.F90 kgen_utils.o calc_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

calc_mod.o: calc_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
