FC := ${FC}
FC_FLAGS := ${FC_FLAGS}
PRERUN := ${PRERUN}

all : run

run : build
	${PRERUN}; ./test.exe

build: test.exe

test.exe: test.o calling_module.o kernel.o
	${PRERUN}; ${FC} -o $@ ${FC_FLAGS} $^

test.o: test_top.F90 calling_module.o
	${PRERUN}; ${FC} -c -o $@ ${FC_FLAGS} $<

calling_module.o: calling_module.F90 kernel.o
	${PRERUN}; ${FC} -c -o $@ ${FC_FLAGS} $<

kernel.o: kernel.F90
	${PRERUN}; ${FC} -c -o $@ ${FC_FLAGS} $<

clean:
	rm -f *.o *.exe *.mod
