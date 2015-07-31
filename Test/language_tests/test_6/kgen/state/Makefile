run: build
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src; ./test6.exe

build: replace
	cd /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src; make build
	mv -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90

replace: save
	cp -f test6_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90

recover_from_locals:
	cp -f test6_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90 /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90.kgen_org; fi
	if [ ! -f test6_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_6/src/test6_mod.F90 test6_mod.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
