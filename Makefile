all: byte-compile pydoc-info

# * Byte-compilation

TESTS = $(shell ./scripts/find-tests)
byte-compile-tests: $(TESTS)

%.elc:%.el
	rm -f $@
	touch $@

byte-compile: byte-compile-tests
	emacs -q -batch --load "init.el" --eval '(byte-recompile-directory "." 0)'

clean:
	find . -name '*.elc' -delete

# * pydoc-info

PYDOC_INFO = share/info/python.info
PYDOC_INFO_BASE = $(shell dirname $${PWD}/$(PYDOC_INFO))
PYDOC_INFO_DIR = $(PYDOC_INFO_BASE)/dir

$(PYDOC_INFO):
	mkdir -p $(PYDOC_INFO_BASE)
	wget https://bitbucket.org/jonwaltman/pydoc-info/downloads/python.info.gz \
	     -O $(PYDOC_INFO).gz
	gunzip $(PYDOC_INFO).gz

$(PYDOC_INFO_DIR): $(PYDOC_INFO)
	install-info --info-dir=$(PYDOC_INFO_BASE) $(PYDOC_INFO)

pydoc-info: $(PYDOC_INFO_DIR)


# Local Variables:
#  eval: (outline-minor-mode 1)
# End:
