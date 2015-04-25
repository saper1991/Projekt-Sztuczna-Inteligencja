#!/usr/bin/env bash

# TODO
# 1. Print dirs so we know what is being checked
# 2. Compare x.c's output with the one corresponding to x.output IF it's specified.
# 3. Clean output

# Clever Timeout technique: http://stackoverflow.com/questions/5161193/bash-script-that-kills-a-child-process-after-a-given-timeout

ALL_TESTS=0
FAILED_TESTS=0

# Timeout.
declare -i timeout=2

for basic_file in $(find . -iname "*.basic")
do
    ALL_TESTS=$[$ALL_TESTS + 1]
    last_failed=0
    ( cmdpid=$BASHPID; (sleep $timeout; last_failed=1; kill $cmdpid &>/dev/null) & exec ../basic2c.pl 2>/dev/null "$basic_file" > tmp.c )

    gcc -w tmp.c &>/dev/null

    if [[ $? -gt 0 ]] || [[ $last_failed -eq 1 ]]
    then    
        echo -e "\e[0;31m [Fail] \e[0m ${basic_file}: Failed." 
        FAILED_TESTS=$[$FAILED_TESTS + 1]
        last_failed=0;
    else
        echo -e "\e[0;32m [OK] \e[0m ${basic_file}: Passed."
    fi

    # Cleanup
    rm a.out 2>/dev/null
    rm tmp.c 2>/dev/null
done

echo -e "\nTotal tests run: ${ALL_TESTS}"
echo "Failed tests: ${FAILED_TESTS}"

echo -en "\e[0m"
