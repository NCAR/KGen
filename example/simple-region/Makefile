KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
FC := ifort
FC_FLAGS := -O3

test:
	${KGEN} \
		-D ROW=4,COLUMN=4 \
		-I ${SRC_DIR} \
		--timing repeat=1000 \
		--invocation 1:2:3 \
		--kernel-compile FC='${FC}',FC_FLAGS='${FC_FLAGS}' \
		--state-build cmds="cd ${SRC_DIR}; make build" \
		--state-run cmds="cd ${SRC_DIR}; make run" \
		${SRC}

clean:
	${MAKE} clean -C src
	rm -rf kernel state kgen.log
