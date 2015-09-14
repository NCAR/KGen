# KGEN Unit Test

include Makefile.inc

SRC_DIR := $(shell find src/ -type d -not -name 'misc' -not -name 'src' -not -name Templates)
TMP_DIR := $(shell find src/ -type d -not -name 'misc' -not -name 'src' -not -name Templates | sed s:src/:tmp_:)
#TEST_ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))/..
#KGEN_ROOT := ${TEST_ROOT}/../kgen
#FC := ifort
#TESTS := $(shell ls -d */src)
#| (read p1; mkdir $$p1); 

all : clean test

test: copy
	@n=0 ; \
	m=0 ; \
	num=`find . -maxdepth 1 -type d -not -name src -not -name . | wc -w` ; \
	for t in ${TMP_DIR}; do \
    		${MAKE} test --no-print-directory -C $$t TEST_DIR=$$t ; \
		if grep -q "TEST $$t passed" ./Results.txt ; then \
			echo Test $$t passed; \
                        rm -rf $$t; \
			let "n+=1" ; \
		else \
			echo Test $$t failed; \
			let "m+=1" ; \
		fi; \
	done; \
	printf "\n************************************************\n" ; \
	echo $$n out of $$num tests passed. $$m FAILED. ; \
	printf "************************************************\n" ; \
	printf "\n$$n/$$num tests passed. $$m FAILED." >> ./Results.txt ;

copy : mktemps
	@for t in ${TMP_DIR}; do \
		cp -f ./Makefile.kgen  $$t/Makefile; \
	done

mktemps:
	@for t in ${TMP_DIR}; do \
		echo $$t | (read p1; mkdir $$p1); \
	done

clean:
	@if [ -e Results.txt ] ; then \
		rm Results.txt; \
	fi;
	@for t in ${TMP_DIR}; do \
		if [ -d $$t ] ; then \
			rm -r $$t; \
		fi; \
	done


