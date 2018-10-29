#KGEN_HOME := $(HOME)/devel/packages/KGen
KGEN_HOME := $(HOME)/tmp/KGen
KGEN := ${KGEN_HOME}/bin/kgen

SRC_DIR := ${PWD}/../src
SRC := ${SRC_DIR}/radtrans/rad_diffusion.f90

# don't have invocations beyond first cycle if the function is only called 1x!
# there is still a race condition somewhere in the src/Makefile. Change to -j8
# until we can find it.

test:
	${KGEN} \
	--check tolerance=1.0D-10 \
	--cmd-build="cd ${SRC_DIR}; make -j8" \
	--cmd-clean="cd ${SRC_DIR}; make clean; cd ${SRC_DIR}/run; ./clean.sh" \
	--cmd-run="cd ${SRC_DIR}; make run" \
	--exclude-ini=exclude.ini \
	--invocation=0-15:0:0 \
	--mpi=enable \
	--repr-etime=disable \
	--verbose=3 \
	${SRC}


#	--invocation 0-383:0:0 \

clean:
	${MAKE} clean -C ../src
	$(RM) -rf  _kgen_compflag_cmdwrapper.sh elapsedtime kernel kgen.log model state 
	#$(RM) include.ini strace.log
