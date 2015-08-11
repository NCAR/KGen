# KGEN Test Makefile
#
#

FC := ifort
CURR_DIR := $(shell pwd)
SRC_DIR := $(shell echo ${CURR_DIR} | sed s:tmp_:src/:)
TMP_DIR := ${CURR_DIR}/tmp_test
SRC := ${TMP_DIR}/calling_module.F90
CALLSITE:=calling_module.calling_subroutine.add
ERR_FILE:=${CURR_DIR}/errors.log
test: gendata
	@cd kernel; make >> ${ERR_FILE} 2>&1;
	@if grep -q "verification PASSED" ${ERR_FILE} ; then \
		echo TEST ${TEST_DIR} passed >> ../Results.txt; \
		${MAKE} clean; \
	else \
		echo TEST ${TEST_DIR} failed. Check errors.log for more details >> ../Results.txt; \
	fi;

gendata: extract
	@cd state; make >> ${ERR_FILE} 2>&1;

extract: copy
	@python ${KGEN_ROOT}/kgen.py \
    -D ROW=4,COLUMN=4 \
    -I ${TMP_DIR} \
    --state-build cmds="cd ${TMP_DIR}; make clean; make build" \
    --state-run cmds="cd ${TMP_DIR}; make run" \
    --kernel-compile FC=${FC} \
    ${SRC}:${CALLSITE} >>${ERR_FILE} 2>&1

copy: mkdir
	@cp -rf ${SRC_DIR}/* ${TMP_DIR}
	@if [ ! -e ${TMP_DIR}/test_top.F90 ] ; then \
		cp -f ${SRC_DIR}/test_top.F90 ${TMP_DIR} >>${ERR_FILE} 2>&1; \
	fi;
	@if [ ! -e ${TMP_DIR}/calling_module.F90  ] ; then \
		cp -f ${SRC_DIR}/calling_module.F90 ${TMP_DIR} >>${ERR_FILE} 2>&1; \
	fi;
	@if [ ! -e ${TMP_DIR}/kernel.F90 ] ; then \
		cp -f ${SRC_DIR}/kernel.F90 ${TMP_DIR} >>${ERR_FILE} 2>&1; \
	fi;
	@if [ ! -e ${TMP_DIR}/Makefile ] ; then \
		cp -f ${SRC_DIR}/Makefile ${TMP_DIR} >>${ERR_FILE} 2>&1; \
	fi;

mkdir:
	@mkdir -p ${TMP_DIR} >${ERR_FILE} 2>&1

clean:
	@rm -rf ${TMP_DIR} kernel state kgen.log
