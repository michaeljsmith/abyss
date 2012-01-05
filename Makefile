EXE=jest

.PHONEY: default
default: exe test

.PHONEY: exe
exe: a.out

.PHONEY: run
run: exe
	./a.out

.PHONEY: test
test: exe
	python test.py

a.out: main.cpp
	clang++ -std=c++0x -o $(EXE) main.cpp
