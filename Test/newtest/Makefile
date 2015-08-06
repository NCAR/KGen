# KGEN Unit Test

SRC_DIR := $(shell find src/ -maxdepth 1 -type d -not -name 'misc' -not -name 'src')
TMP_DIR := $(shell find src/ -maxdepth 1 -type d -not -name 'misc' -not -name 'src' | sed s:src/:tmp_:)
TEST_ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))/..
#KGEN_ROOT := /glade/p/work/amogh/kgen_dev/kgen/trunk/src

#${MAKE} test -C $$t TEST_ROOT=${TEST_ROOT} KGEN_ROOT=${KGEN_ROOT} SRC_DIR=${SRC_DIR} || exit 1; done
#TESTS := $(shell ls -d */src)

all : clean test

test: copy
	@for t in ${TMP_DIR}; do \
    		${MAKE} test --no-print-directory -C $$t TEST_ROOT=${TEST_ROOT} KGEN_ROOT=${KGEN_ROOT} TEST_DIR=$$t ; \
		if grep -q "TEST $$t passed" ./Results.txt ; then \
			echo Test $$t passed; \
			rm -rf $$t; \
		else \
			echo Test $$t failed; \
		fi; \
	done

copy : mktemps
	@for t in ${TMP_DIR}; do \
		cp -f src/misc/Makefile $$t; \
	done

mktemps:
	@for t in ${TMP_DIR}; do \
		echo $$t | (read p1; mkdir $$p1); \
	done
clean:
	-rm -r tmp_* Results.txt
