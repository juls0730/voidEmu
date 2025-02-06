#!/bin/bash

TIMEOUT=2

if [ ! -d tests/extern ]; then
    mkdir -p tests/extern
fi

extern_roms=(
    "https://github.com/Timendus/chip8-test-suite/raw/main/bin/1-chip8-logo.ch8"
    "https://github.com/Timendus/chip8-test-suite/raw/main/bin/2-ibm-logo.ch8"
    "https://github.com/Timendus/chip8-test-suite/raw/main/bin/3-corax+.ch8"
    "https://github.com/Timendus/chip8-test-suite/raw/main/bin/4-flags.ch8"
)

for rom in "${extern_roms[@]}"; do
    file="tests/extern/$(basename "$rom")"
    if [ ! -f "$file" ]; then
        echo "Downloading $rom"
        curl -L "$rom" -o "$file"
    fi
done

if [ "$1" == "--download-only" ]; then
    exit 0
fi

roms=($(find tests/ -type f -name "*.ch8"))
testOutput=$(mktemp)

if ${VERBOSE:-false}; then
    testOutput=2
fi

for rom in "${roms[@]}"; do
    echo -n "$rom "
    ./bin/voidEmu "$rom" $@ 1>&$testOutput &
    pid=$!

    SECONDS=0
    while kill -0 $pid 2>/dev/null && [ $SECONDS -lt $TIMEOUT ]; do
        sleep 0.1
    done

    if ! kill -0 $pid 2>/dev/null; then
        wait $pid
        exit_code=$?

        if [ $exit_code -eq 0 ]; then
            echo -e "$rom \x1b[97m[\x1b[1;32mPASS\x1b[97m]\x1b[0m (Exited Successfully)"
            continue 
        else
            echo -e "$rom \x1b[97m[\x1b[1;31mFAIL\x1b[97m]\x1b[0m (Exited with code $exit_code)"
            echo "Output:"
            cat $testOutput
            exit 1
        fi
    fi

    echo "Press Enter to confirm PASS, or Ctrl-C to FAIL..."
    read -r

    if kill -0 $pid 2>/dev/null; then
        kill $pid
        wait $pid 2>/dev/null
    fi

    echo -e "$rom \x1b[97m[\x1b[1;32mPASS\x1b[97m]\x1b[0m"
done
