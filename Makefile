RSFLAGS=-O

pd : main.scala Makefile
	scalac main.scala

TESTS=$(sort $(wildcard *.fun))
RUNS=$(patsubst %.fun,%.result,$(TESTS))

test : $(RUNS)

$(RUNS) : %.result : %.fun Makefile pd
	@echo -n "[$*] ... "
	@scala Interpreter $*.fun > $*.out
	@((diff -b $*.out $*.ok > /dev/null) && echo "pass") || (echo "fail" ; echo "--- expected ---"; cat $*.ok; echo "--- found ---" ; cat $*.out)

clean :
	rm -f *.class
	rm -f *.out
	rm -f pc

