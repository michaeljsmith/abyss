EXE=jest

.PHONEY: exe
exe: $(EXE)

.PHONEY: run
run: exe
	./a.out

$(EXE): main.cpp
	g++ -o $(EXE) -lffi main.cpp
