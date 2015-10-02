run: build
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test; make run

build: replace
	cd /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test; make clean; make build
	mv -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90
	mv -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90

replace: save
	cp -f types_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90
	cp -f calling_module.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90

recover:
	cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90
	cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90

recover_from_locals:
	cp -f types_mod.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90
	cp -f calling_module.F90.kgen_org /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90

save:
	if [ ! -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90.kgen_org; fi
	if [ ! -f types_mod.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/types_mod.F90 types_mod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90 /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90.kgen_org; fi
	if [ ! -f calling_module.F90.kgen_org ]; then cp -f /glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/tmp_complex_datatypes/tmp_test/calling_module.F90 calling_module.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
