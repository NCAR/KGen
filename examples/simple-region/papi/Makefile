# Makefile for KGEN-generated instrumentation


run: build
	cd /glade/u/home/youngsun/repos/github/KGen/examples/simple-region; cd /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src; make run

build: replace
	cd /glade/u/home/youngsun/repos/github/KGen/examples/simple-region; cd /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src; make build
	mv -f /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90.kgen_org /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90

replace: save
	cp -f update_mod.F90 /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90

recover:
	cp -f update_mod.F90.kgen_org /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90

recover_from_srcdir:
	cp -f /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90.kgen_org /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90

save:
	if [ ! -f /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90.kgen_org ]; then cp -f /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90 /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90.kgen_org; fi
	if [ ! -f update_mod.F90.kgen_org ]; then cp -f /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src/update_mod.F90 update_mod.F90.kgen_org; fi

clean:
	cd /glade/u/home/youngsun/repos/github/KGen/examples/simple-region/src; make clean

