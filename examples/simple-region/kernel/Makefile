# Makefile for KGEN-generated kernel

FC_0 := /ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016/linux/bin/intel64/ifort

ALL_OBJS := update_mod.o calc_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

run: build
	./kernel.exe

papi: build-papi
	./kernel.exe

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  

build-papi: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  /glade/apps/opt/papi/5.3.2/intel/15.0.0/lib/libpapi.a 

update_mod.o: update_mod.F90 calc_mod.o kgen_utils.o tprof_mod.o
ifeq (${MAKECMDGOALS}, papi)
	/usr/bin/cpp -w -traditional -P  -DKGEN_PAPI -I/glade/apps/opt/papi/5.3.2/intel/15.0.0/include $< tmp.$<
else
	/usr/bin/cpp -w -traditional -P $< tmp.$<
endif
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ tmp.$<
	rm -f tmp.$<

calc_mod.o: calc_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 update_mod.o calc_mod.o kgen_utils.o tprof_mod.o
ifeq (${MAKECMDGOALS}, papi)
	/usr/bin/cpp -w -traditional -P  -DKGEN_PAPI -I/glade/apps/opt/papi/5.3.2/intel/15.0.0/include $< tmp.$<
else
	/usr/bin/cpp -w -traditional -P $< tmp.$<
endif
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ tmp.$<
	rm -f tmp.$<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
