ROW := 4
COL := 4

# uncommand followings to build with PAPI library
#PAPI_DIR := /path/to/PAPI
#PAPI_LIB := ${PAPI_DIR}/lib/libpapi.a

FC := ifort
FC_FLAGS := -DROW=${ROW} -DCOL=${COL}

run: build
	./demo.exe

build: demo.exe

demo.exe: program.o update_mod.o calc_mod.o
	${FC} -o $@ $^ ${PAPI_LIB}

program.o: program.F90 update_mod.o
	${FC} -c -o $@ $<

update_mod.o: update_mod.F90 calc_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

calc_mod.o: calc_mod.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.exe *.o *.mod
