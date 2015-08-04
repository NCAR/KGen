FC := ifort

all : run

run : build
	./test2.exe

build: test2.exe

test2.exe: test2.o test2_mod.o test2_type_mod.o test2_compute.o
	${FC} -o $@ $^

test2.o: test2.F90 test2_mod.o
	${FC} -c -o $@ $<

test2_mod.o : test2_mod.F90 test2_type_mod.o test2_compute.o
	${FC} -c -o $@ $<

test2_compute.o : test2_compute.F90 test2_type_mod.o
	${FC} -c -o $@ $<

test2_type_mod.o : test2_type_mod.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
