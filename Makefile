all: byte-compile

TESTS = $(shell ./scripts/find-tests)

byte-compile-tests: $(TESTS)

%.elc:%.el
	rm -f $@
	touch $@

byte-compile: byte-compile-tests
	emacs -q -batch --load "init.el" --eval '(byte-recompile-directory "." 0)'


clean:
	find . -name '*.elc' -delete