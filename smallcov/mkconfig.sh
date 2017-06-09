#!/bin/bash

touch cov.config
echo "--program manifest.txt" >> cov.config
grep "pos-tests" configuration-default >> cov.config
grep "neg-tests" configuration-default >> cov.config
echo "--testscript ./test.sh" >> cov.config
echo "--prefix preprocessed" >> cov.config
echo "--compcmd ./compile.sh __EXE_NAME__ >& /dev/null" >> cov.config
diffs=`find diffs/ -follow | grep "\-diff"`
echo "--diffs $diffs" >> cov.config

/opt/smallcov/smallcov cov.config
cat coveringtests.txt
