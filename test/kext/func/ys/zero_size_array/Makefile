FC := ifort

all : run

run : build
	./test.exe

build: test.exe

test.exe: test.o calling_module.o kernel.o
	${FC} -O0 -o $@ $^

test.o: test_top.F90 calling_module.o
	${FC} -O0 -c -o $@ $<

calling_module.o: calling_module.F90 kernel.o
	${FC} -O0 -c -o $@ $<

kernel.o: kernel.F90
	${FC} -O0 -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
