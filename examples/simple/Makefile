KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90
CALLSITE := update_mod:update:calc

test:
	${KGEN} \
		--timing repeat=100 \
		--invocation 0:0:1,0:0:2,0:0:3 \
		--check tolerance=1.0D-14 \
		--rebuild all \
		--cmd-clean "cd ${SRC_DIR}; make clean" \
		--cmd-build "cd ${SRC_DIR}; make build" \
		--cmd-run "cd ${SRC_DIR}; make run" \
		${SRC}:${CALLSITE} 
#--source alias=/path/a:path/b # add this option if your system requires alias paths

clean:
	${MAKE} clean -C src
	rm -rf kernel state kgen.log strace.log include.ini _kgen_compflag_cmdwrapper.sh
