#!/bin/bash

#KGEN_HOME=/glade/p/work/amogh/kgen_dev/kgen/trunk/src
#KGEN_HOME=${HOME}/trepo/prjs/share/kgen4/src
#KGEN=${KGEN_HOME}/kgen.py

CURR_DIR=`pwd`
#CESM_HOME=/glade/p/work/amogh/cesm1_3_beta15 
#SRC_DIR=/glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_4/src
#SRC=${SRC_DIR}/test4_mod.F90
#BUILD_DIR=${SRC_DIR}
#CALLSITE=test4_mod.test_4.add

python ${KGEN} \
	-i include.ini \
	-e exclude.ini \
	--state-build cmds="cd ${BUILD_DIR}; make build" \
	--state-run cmds="cd ${BUILD_DIR}; ./test4.exe" \
	${SRC}:${CALLSITE}
	#-D HAVE_CONFIG_H=1 \
	#--ordinal-numbers 1,10,20 \
	#--mpi ranks=0 \
	#--kernel-compile FC_FLAGS='-assume byterecl -fp-model precise -ftz -O3 -g -openmp' \
	#--debug logging.select.name=buffer \

