FC := ifort

all : run

run : build
	./test7.exe

build: test7.exe

test7.exe: test7.o test7_mod.o compute.o
	${FC} -o $@ $^

test7.o: test7.F90 test7_mod.o
	${FC} -c -o $@ $<

test7_mod.o : test7_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o : compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
