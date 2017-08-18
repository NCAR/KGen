
KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

PAPI_DIR := /glade/apps/opt/papi/5.3.2/intel/15.0.0

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/update_mod.F90

test:
	${KGEN} \
		--cmd-clean "cd ${SRC_DIR}; make clean" \
		--cmd-build "cd ${SRC_DIR}; make build" \
		--cmd-run "cd ${SRC_DIR}; make run" \
        --repr-code enable \
        --repr-etime nbins=5,ndata=20 \
 --repr-papi header=${PAPI_DIR}/include/f90papi.h,static=${PAPI_DIR}/lib/libpapi.a,event=PAPI_TOT_INS \
		--check tolerance=1.0D-14 \
		--rebuild all \
		${SRC}

# --source alias=/path/a:path/b # add this option if your system requires alias paths \
# --repr-papi header=${PAPI_DIR}/include/f90papi.h,static=${PAPI_DIR}/lib/libpapi.a,event=PAPI_TOT_INS \

clean:
	${MAKE} clean -C src
	rm -rf kernel state kgen.log strace.log include.ini _kgen_compflag_cmdwrapper.sh model model.ini elapsedtime coverage papi
