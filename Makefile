CXX := g++
CXXFLAGS := -Wall -Wextra -std=c++23 -g -Werror -Ilibs
LDFLAGS := `sdl2-config --libs`

BIN_DIR := bin

all: voidEmu assembler disassembler

run: all
	./bin/voidEmu $(FILE)

assembler: $(wildcard assembler/*.cpp) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $^ -o ${BIN_DIR}/$@

disassembler: $(wildcard disassembler/*.cpp) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $^ -o ${BIN_DIR}/$@

voidEmu: $(wildcard src/*.cpp) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS)  `sdl2-config --cflags` $^ -o ${BIN_DIR}/$@ $(LDFLAGS)

$(BIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(BIN_DIR)
	rm -rf tests/extern

.PHONY: all clean run
