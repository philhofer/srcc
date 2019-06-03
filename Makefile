CSC:=csc
CSCFLAGS:=-static -O3
TOOL:=ss6c

%.o %.link %.import.scm: %.scm
	$(CSC) $(CSCFLAGS) -unit $* -J -c $< -o $*.o

%.full.o %.link: %.scm
	$(CSC) $(CSCFLAGS) -unit $* -c $< -o $*.full.o

# libraries imported by main
LIBRARIES=s6-rc.o

# module environment for evaluated scripts
SCRIPTLIBS=interface.full.o

$(TOOL): main.scm $(LIBRARIES) $(SCRIPTLIBS)
	$(CSC) $(CSCFLAGS) $^ -o $@

.PHONY: clean
clean:
	$(RM) *.so *.o *.link *.import.scm

.PHONY: test
test: $(TOOL) test/test.sh test/*.scm
	@cd test && ./test.sh
