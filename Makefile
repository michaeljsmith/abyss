.PHONEY: exe
exe: a.out

.PHONEY: run
run: exe
	./a.out

a.out: main.cpp
	g++ -g -Wall -Wextra -Werror -std=c++0x -o a.out -lglfw -lGLU main.cpp
