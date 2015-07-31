# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := 

ALL_OBJS := kernel_driver.o test2_mod.o kgen_utils.o test2_compute.o test2_type_mod.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 test2_mod.o kgen_utils.o test2_compute.o test2_type_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

test2_mod.o: test2_mod.F90 kgen_utils.o test2_compute.o test2_type_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

test2_compute.o: test2_compute.F90 kgen_utils.o test2_type_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

test2_type_mod.o: test2_type_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
