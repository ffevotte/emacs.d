all: byte-compile pydoc-info

# * Byte-compilation

byte-compile: bc-packages bc-emacsd

bc-packages:
	chmod -R u+w share/elisp

	@echo "Installing packages"
	rsync -av                                     \
	  -f '- *tests.el'                            \
	  -f '- *test.el'                             \
	  -f '- features/support/*'                   \
	  -f 'P *.elc' -f '+ */' -f '+ *.el' -f '- *' \
	  --prune-empty-dirs --delete-excluded        \
	  packages/ share/elisp/
	@
	@echo Byte-compiling packages
	emacs -q -batch --load "init.el" \
	  --eval '(byte-recompile-directory "share/elisp" 0)'

	chmod -R a-w share/elisp

bc-emacsd:
	emacs -q --batch --load "init.el" \
	  --load "bin/byte-compile.el"    \
	  --eval '(byte-recompile-directory-non-recursive "./")'

clean: bc-clean
bc-clean:
	find . -name '*.elc' -delete

distclean: bc-distclean
bc-distclean:
	$(RM) share/elisp


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

distclean: pydoc-distclean
pydoc-distclean:
	$(RM) $(PYDOC_INFO_BASE)


# Local Variables:
#  eval: (outline-minor-mode 1)
# End:
