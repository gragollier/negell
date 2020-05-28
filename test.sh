#!/bin/sh

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

cabal build
negell=$(find . -type f -executable -name "negell")

for f in tests/*.nega;
do
    testname=$(echo $f | cut -f 1 -d '.' )
    printf "Running test $testname.nega..."

    cat $testname.input | $negell $testname.nega > $testname.out
    diff -w $testname.out $testname.expected
    returnCode=$?

    [ $returnCode -eq 0 ] && echo "${GREEN}PASSED${NC}" || echo "${RED}FAILED${NC}"
    rm $testname.out
done
