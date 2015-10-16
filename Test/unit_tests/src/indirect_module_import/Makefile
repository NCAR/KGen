FC := ifort

all : run

run : build
	./test.exe

build: test.exe

test.exe: test.o calling_module.o kernel.o
	${FC} -o $@ $^

test.o: test_top.F90 calling_module.o
	${FC} -c -o $@ $<

calling_module.o: calling_module.F90 module1.o
	${FC} -c -o $@ $<

module1.o: module1.F90 module2.o
	${FC} -c -o $@ $<

module2.o: module2.F90 kernel.o
	${FC} -c -o $@ $<

kernel.o: kernel.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.o *.exe *.mod
