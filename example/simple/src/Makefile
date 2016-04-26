ROW := 4
COL := 4

FC ?= gfortran
FC_FLAGS := -DROW=${ROW} -DCOL=${COL}

run: build
	./demo.exe

build: demo.exe

demo.exe: program.o update_mod.o calc_mod.o
	${FC} -o $@ $^

program.o: program.F90 update_mod.o
	${FC} -c -o $@ $<

update_mod.o: update_mod.F90 calc_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

calc_mod.o: calc_mod.F90
	${FC} -c -o $@ $<

clean:
	rm -f *.exe *.o *.mod
