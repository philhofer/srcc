CSC:=csc
CSCFLAGS:=-static -O3
TOOL:=srcc

%.o %.link %.import.scm: %.scm
	$(CSC) $(CSCFLAGS) -unit $* -J -c $< -o $*.o

LIBRARIES=s6-rc.o interface.o

$(TOOL): main.scm $(LIBRARIES)
	$(CSC) $(CSCFLAGS) $^ -o $@

.PHONY: clean
clean:
	$(RM) $(TOOL) *.so *.o *.link *.import.scm

.PHONY: test
test: $(TOOL) test/test.sh test/*.scm
	@cd test && ./test.sh
