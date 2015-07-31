run: build
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src; ./test1.exe

build: replace
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src; make build
	mv -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90

replace: save
	cp -f test1_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90

recover_from_locals:
	cp -f test1_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90.kgen_org; fi
	if [ ! -f test1_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/src/test1_mod.F90 test1_mod.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
