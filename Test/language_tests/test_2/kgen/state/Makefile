run: build
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src; ./test2.exe

build: replace
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src; make build
	mv -f /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90
	mv -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90

replace: save
	cp -f test2_type_mod.F90 /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90
	cp -f test2_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90
	cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90

recover_from_locals:
	cp -f test2_type_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90
	cp -f test2_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90 /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90.kgen_org; fi
	if [ ! -f test2_type_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/regression_tests/unit_tests/test_2/src/test2_type_mod.F90 test2_type_mod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90.kgen_org; fi
	if [ ! -f test2_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/src/test2_mod.F90 test2_mod.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
