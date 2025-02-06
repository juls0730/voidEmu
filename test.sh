#!/bin/bash

roms=($(find tests/ -type f))

testOutput=$(mktemp)

if ${VERBOSE:-false}; then
    testOutput=2
fi

for rom in "${roms[@]}"; do
    echo -n "$rom "
    ./bin/voidEmu $rom 1>&$testOutput
    if [ $? -ne 0 ]; then
        echo -en "\x1b[2K\r"
        cat $testOutput
        echo -e "$rom \x1b[97m[\x1b[1;31mFAIL\x1b[97m]\x1b[0m"
        exit 1
    else
        echo -e "\x1b[97m[\x1b[1;32mPASS\x1b[97m]\x1b[0m"
    fi
done
