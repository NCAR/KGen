KGEN_HOME := ../../kgen
KGEN := ${KGEN_HOME}/kgen.py

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc
FC := mpiifort
FC_FLAGS := -O3


test:
	python ${KGEN} \
		-D ROW=4,COL=4 \
		-I ${SRC_DIR} \
		-e exclude.ini \
		--mpi ranks=0:1 \
		--invocation 1,3 \
		--kernel-compile FC='${FC}',FC_FLAGS='${FC_FLAGS}' \
		--timing repeat=1000 \
		--state-build cmds="cd ${SRC_DIR}; make build" \
		--state-run cmds="cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE}

clean:
	rm -rf kernel state kgen.log
