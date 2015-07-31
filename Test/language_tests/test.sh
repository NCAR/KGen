#! /bin/bash

CURR_DIR=`pwd`

echo running test 1
cd test_1
./test.sh
cd ${CURR_DIR}

echo running test 2
cd test_2
./test.sh
cd ${CURR_DIR}

echo running test 3
cd test_3
./test.sh
cd ${CURR_DIR}

echo running test 4
cd test_4
./test.sh
cd ${CURR_DIR}

echo running test 5
cd test_5
./test.sh
cd ${CURR_DIR}

echo running test 6
cd test_6
./test.sh
cd ${CURR_DIR}

echo running test 7
cd test_7
./test.sh
cd ${CURR_DIR}
