run: build
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src; make run

build: replace
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src; make build
	mv -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90

replace: save
	cp -f update_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90

recover_from_locals:
	cp -f update_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90.kgen_org; fi
	if [ ! -f update_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/src/update_mod.F90 update_mod.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
