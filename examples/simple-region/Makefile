
KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

#PAPI_DIR := /path/to/PAPI

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90

test:
	${KGEN} \
		--cmd-clean "cd ${SRC_DIR}; make clean" \
		--cmd-build "cd ${SRC_DIR}; make build" \
		--cmd-run "cd ${SRC_DIR}; make run" \
		${SRC}

# additional flags
#        --repr-code enable \
#        --repr-etime nbins=5,ndata=20 \
#        --repr-papi header=${PAPI_DIR}/include/f90papi.h,static=${PAPI_DIR}/lib/libpapi.a,event=PAPI_TOT_INS \
#        --source alias=/path/a:path/b # add this option if your system requires alias paths \
#        --repr-papi header=${PAPI_DIR}/include/f90papi.h,static=${PAPI_DIR}/lib/libpapi.a,event=PAPI_TOT_INS \
#        --check tolerance=1.0D-14 \
#        --rebuild all \

clean:
	${MAKE} clean -C src
	rm -rf kernel state kgen.log strace.log include.ini _kgen_compflag_cmdwrapper.sh model model.ini elapsedtime coverage papi
