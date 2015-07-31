FC := ifort

all : run

run : build
	./test3.exe

build: test3.exe

test3.exe: test3.o test3_mod.o compute.o
	${FC} -o $@ $^

test3.o: test3.F90 test3_mod.o
	${FC} -c -o $@ $<

test3_mod.o : test3_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o : compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
