#!/bin/bash

if [ ! -d tests/extern ]; then
    mkdir tests/extern
fi

extern_roms=("https://github.com/Timendus/chip8-test-suite/raw/main/bin/1-chip8-logo.ch8" "https://github.com/Timendus/chip8-test-suite/raw/main/bin/2-ibm-logo.ch8")
for rom in "${extern_roms[@]}"; do
    if [ ! -f tests/extern/$(basename $rom) ]; then
        echo "Downloading $rom"
        curl -L $rom -o tests/extern/$(basename $rom)
    fi
done

roms=($(find tests/ -type f -name "*.ch8"))

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
