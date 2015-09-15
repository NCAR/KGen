KGEN_HOME := ../../kgen
KGEN := ${KGEN_HOME}/kgen.py

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc
FC := ifort
FC_FLAGS := -O3

test:
	python ${KGEN} \
		-D ROW=4,COLUMN=4 \
		-I ${SRC_DIR} \
		--timing repeat=1000 \
		--kernel-compile FC='${FC}',FC_FLAGS='${FC_FLAGS}' \
		--state-build cmds="cd ${SRC_DIR}; make build" \
		--state-run cmds="cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE}

clean:
	rm -rf kernel state kgen.log
