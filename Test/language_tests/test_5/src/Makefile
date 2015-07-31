FC := ifort

all : run

run : build
	./test5.exe

build: test5.exe

test5.exe: test5.o test5_mod.o compute.o
	${FC} -o $@ $^

test5.o: test5.F90 test5_mod.o
	${FC} -c -o $@ $<

test5_mod.o : test5_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o : compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
