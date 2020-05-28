#!/bin/sh

cabal build
negell=$(find dist-newstyle/build -type f -executable -name "negell")

for f in tests/*.nega;
do
    testname=$(echo $f | cut -f 1 -d '.' )
    printf "Running test $testname.nega..."

    cat $testname.input | $negell $testname.nega > $testname.out
    diff -w $testname.out $testname.expected
    returnCode=$?

    [ $returnCode -eq 0 ] && echo "PASSED" || echo "FAILED"
    rm $testname.out
done
