all:
	csc -lSDL2 -J invert.scd
	csc -library invert.scd
	csc -library invert.import.scm
	csc -lSDL2 main.scm -o main
run:
	./main
