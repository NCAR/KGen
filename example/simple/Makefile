#KGEN_HOME := /glade/u/tdd/asap/contrib/kgen/std/lib
KGEN_HOME := ../../kgen
KGEN := ${KGEN_HOME}/kgen.py

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc
MPI_HOME := /ncar/opt/intel/psxe-2015/impi/5.0.1.035/intel64
MPI_LIB := ${MPI_HOME}/lib
MPI_INC := ${MPI_HOME}/include
FC := mpiifort

test:
	python ${KGEN} \
		-D ROW=4,COLUMN=4 \
		-I ${SRC_DIR}:${MPI_INC} \
		--timing repeat=1000 \
		--kernel-compile FC=${FC},FC_FLAGS='-O3' \
		--state-build cmds="cd ${SRC_DIR}; make build" \
		--state-run cmds="cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE}
		#--kernel-compile FC_FLAGS='-O3' \

clean:
	rm -rf kernel state kgen.log
