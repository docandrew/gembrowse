all:
	mkdir -p obj
	gprbuild

clean:
	-rm obj/*.o
	-rm obj/gembrowse