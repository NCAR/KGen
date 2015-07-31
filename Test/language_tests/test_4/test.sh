#!/bin/bash

#This is the script for making automatically testing new versions of kgen
#This script uses a canonical version of kgen to extract a kernel that uses certain
#features of fortran that have been known to cause kgen problem. The same kernel is
#then extracted using the new version of kgen. The stdouts and verification of both
#kernels are compared to check if the differences in the rms difference of both the
#versions of kgen are within acceptable limits

CURR_DIR=`pwd`
ORIG_TESTS=${CURR_DIR}/orig_tests
#SRC_DIR=${CURR_DIR}/src
KGEN_TEST=${CURR_DIR}/kgen
TEST_KGEN_SCRIPT=${KGEN_TEST}/test_kgen.sh
LOG_FILE=${CURR_DIR}/errors.log

export SRC_DIR=${CURR_DIR}/src
export SRC=${SRC_DIR}/test4_mod.F90
export BUILD_DIR=${SRC_DIR}
export CALLSITE=test4_mod.test_4.add
#export KGEN_HOME=/glade/p/work/amogh/kgen_dev/kgen/trunk/src
#export KGEN=${KGEN_HOME}/kgen.py

${CURR_DIR}/allclean.sh

cd ${KGEN_TEST}
${TEST_KGEN_SCRIPT} >  ${LOG_FILE} 2>&1

if [ $? -eq 0 ]; then
        echo test kgen extraction successful
else
        echo test kgen extraction failed. Check errors.log
        exit
fi

cd ${KGEN_TEST}/state >> ${LOG_FILE} 2>&1

make >> ${LOG_FILE} 2>&1

if [ $? -eq 0 ]; then
        echo test kgen state make successful
else
        echo test kgen state make failed. Check errors.log
        exit
fi

cd ${KGEN_TEST}/kernel >> ${LOG_FILE} 2>&1

make >> ${LOG_FILE} 2>&1

if [ $? -eq 0 ]; then
        echo known kgen kernel make successful
	echo TEST PASSED
else
        echo known kgen kernel make failed. Check errors.log
        exit
fi

#diff ${KGEN_KNOWN}/kernel/output.txt ${KGEN_TEST}/kernel/output.txt


