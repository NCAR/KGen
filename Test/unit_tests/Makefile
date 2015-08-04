
include Makefile.inc

subdirs = $(shell ls -d */)

all : 
	@for s in ${subdirs}; do \
	echo "testing $$s"; \
	${MAKE} all -C $$s; \
 	done

clean :
	@for s in ${subdirs}; do \
        echo "cleaning up $$s"; \
        ${MAKE} clean -C $$s; done

test1 :
	${MAKE} all -C test_1

test2 :
	${MAKE} all -C test_2

test3 :
	${MAKE} all -C test_3

test4 :
	${MAKE} all -C test_4

test5 :
	${MAKE} all -C test_5

test6 :
	${MAKE} all -C test_6

test7 :
	${MAKE} all -C test_7

