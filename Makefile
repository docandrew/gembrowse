all:
	mkdir -p obj
	gprbuild

.PHONY : release
release:
	mkdir -p obj
	gprbuild -Xbuild=release

clean:
	-rm obj/*.o
	-rm obj/gembrowse
	-rm obj/tests

.PHONY : prove
prove:
	gnatprove -Pgembrowse.gpr --level=4 --memlimit=4000
