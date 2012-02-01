EXE=jest

.PHONEY: exe
exe: $(EXE)

.PHONEY: run
run: exe
	./a.out
$(EXE): main.cpp
	g++ -std=c++0x -o $(EXE) main.cpp
