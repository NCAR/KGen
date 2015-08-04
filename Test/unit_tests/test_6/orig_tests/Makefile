FC := ifort

all : run

run : build
	./test6.exe

build: test6.exe

test6.exe: test6.o test6_mod.o compute.o
	${FC} -o $@ $^

test6.o: test6.F90 test6_mod.o
	${FC} -c -o $@ $<

test6_mod.o : test6_mod.F90 compute.o
	${FC} -c -o $@ $<

compute.o : compute.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
