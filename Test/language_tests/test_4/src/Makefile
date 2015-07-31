FC := ifort

all : run

run : build
	./test4.exe

build: test4.exe

test4.exe: test4.o test4_mod.o compute.o
	${FC} -o $@ $^

test4.o: test4.F90 test4_mod.o
	${FC} -c -o $@ $<

test4_mod.o : test4_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o : compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
