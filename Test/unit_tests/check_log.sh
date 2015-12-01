for f in `ls -d tmp_*/`; do echo $f; cat $f/errors.log | tail ; done
