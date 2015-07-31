#!/bin/bash

CURR_DIR=`pwd`
ORIG_TESTS=${CURR_DIR}/orig_tests
INUSE_TESTS=${CURR_DIR}/src
KGEN_TEST=${CURR_DIR}/kgen

echo cleaning up source directory
rm -r ${INUSE_TESTS}/*

echo copying back unit test
cp -f ${ORIG_TESTS}/* ${INUSE_TESTS}/

echo cleaning up kgen directory
#echo ${KGEN_TEST}

cd ${KGEN_TEST}
./cleanup.sh

cd ${CURR_DIR}

