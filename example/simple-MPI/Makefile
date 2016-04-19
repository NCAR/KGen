KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc
FC := ifort
FC_FLAGS := -O3
MPI_INC := /ncar/opt/intel/12.1.0.233/impi/4.0.3.008/intel64/include

test:
	${KGEN} \
		-D ROW=4,COL=4 \
		-i include.ini \
		-I ${SRC_DIR}:${MPI_INC} \
		-e exclude.ini \
		--mpi enable \
		--invocation 0:0:1,0:0:3,1:0:1,1:0:3 \
		--kernel-compile FC='${FC}',FC_FLAGS='${FC_FLAGS}' \
		--timing repeat=1000 \
		--state-build cmds="cd ${SRC_DIR}; make -f Makefile.mpirun build" \
		--state-run cmds="cd ${SRC_DIR}; make -f Makefile.mpirun run" \
		${SRC}:${CALLSITE}

clean:
	${MAKE} -f Makefile.mpirun clean -C src
	rm -rf kernel state kgen.log
