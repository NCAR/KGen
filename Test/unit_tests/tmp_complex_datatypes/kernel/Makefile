# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := 

ALL_OBJS := kernel_driver.o calling_module.o kgen_utils.o types_mod.o kernel.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 calling_module.o kgen_utils.o types_mod.o kernel.o
	${FC} ${FC_FLAGS} -c -o $@ $<

calling_module.o: calling_module.F90 kgen_utils.o kernel.o
	${FC} ${FC_FLAGS} -c -o $@ $<

types_mod.o: types_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel.o: kernel.F90 kgen_utils.o types_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
