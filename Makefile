CXX := g++
CXXFLAGS := -Wall -Wextra -std=c++23 -g -Werror -Ilibs `sdl2-config --cflags`
LDFLAGS := `sdl2-config --libs`

BIN_DIR := bin

all: voidEmu disassembler

run: all
	./bin/voidEmu $(FILE)

disassembler: $(wildcard disassembler/*.cpp) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $^ -o ${BIN_DIR}/$@

voidEmu: $(wildcard src/*.cpp) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $^ -o ${BIN_DIR}/$@ $(LDFLAGS)

$(BIN_DIR) $(OBJ_DIR):
	mkdir -p $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

.PHONY: all clean run
