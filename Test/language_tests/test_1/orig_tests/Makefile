FC := ifort

all : run

run : build
	./test1.exe

build: test1.exe

test1.exe: test1.o test1_mod.o compute.o
	${FC} -o $@ $^

test1.o: test1.F90 test1_mod.o
	${FC} -c -o $@ $<

test1_mod.o: test1_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o: compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
