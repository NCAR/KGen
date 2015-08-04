run: build
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src; ./test4.exe

build: replace
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src; make build
	mv -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90

replace: save
	cp -f test4_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90

recover_from_locals:
	cp -f test4_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90.kgen_org; fi
	if [ ! -f test4_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_4/src/test4_mod.F90 test4_mod.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
