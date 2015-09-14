#KGEN_HOME := /glade/u/tdd/asap/contrib/kgen/std/lib
KGEN_HOME := ../../kgen
#KGEN_HOME := ${HOME}/trepo/prjs/share/kgen4/src
KGEN := ${KGEN_HOME}/kgen.py

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc

test:
	python ${KGEN} \
		-D ROW=4,COL=4 \
		-I /ncar/opt/intel/psxe-2015/impi/5.0.1.035/intel64/include:${SRC_DIR}/ \
		-e exclude.ini \
		--mpi ranks=0:1 \
		--invocation 1,3 \
		--kernel-compile FC_FLAGS='-O3' \
		--timing repeat=1000 \
		--state-build cmds="cd ${SRC_DIR}; make build" \
		--state-run cmds="cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE}

clean:
	rm -rf kernel state kgen.log
