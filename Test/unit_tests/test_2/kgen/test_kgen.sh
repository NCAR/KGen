#!/bin/bash

#KGEN_HOME=/glade/p/work/amogh/kgen_dev/kgen/trunk/src
#KGEN=${KGEN_HOME}/kgen.py

CURR_DIR=`pwd`
#SRC_DIR=/glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src
#SRC=${SRC_DIR}/test2_mod.F90
#BUILD_DIR=${SRC_DIR}
#CALLSITE=test2_mod.test_2.compute

python ${KGEN} \
	-i include.ini \
	-e exclude.ini \
	--state-build cmds="cd ${BUILD_DIR}; make build" \
	--state-run cmds="cd ${BUILD_DIR}; ./test2.exe" \
	${SRC}:${CALLSITE}
