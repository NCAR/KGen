KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90

test:
	${KGEN} \
        --timing repeat=10 \
        --invocation 0:0:1,0:0:2,0:0:3 \
        --check tolerance=1.0D-14 \
        --rebuild all \
        ${SRC} \
        "cd ${SRC_DIR}; make clean" \
        "cd ${SRC_DIR}; make build" \
        "cd ${SRC_DIR}; make run"

clean:
	${MAKE} clean -C src
	rm -rf kernel state kgen.log strace.log include.ini _kgen_compflag_cmdwrapper.sh


