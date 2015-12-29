#This file contains all the macros that are used
#for running KGen's unit tests. The purpose is to allow
#the user to modify necessary variables from one source.

FC := ifort

TEST_ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))) 2>/dev/null)/..
KGEN_ROOT := ${TEST_ROOT}/..

