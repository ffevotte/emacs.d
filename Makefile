all:


# * Directories

all: directories
directories:
	mkdir -p var


# * Byte-compilation

all: byte-compile
byte-compile: bc-packages bc-emacsd

PKG_INSTALL_DIR = share/elisp

bc-packages:
	mkdir -p $(PKG_INSTALL_DIR)
	chmod -R u+w $(PKG_INSTALL_DIR)

	@echo "Installing packages"
	rsync -av                                     \
	  -f '- *tests.el'                            \
	  -f '- *test.el'                             \
	  -f '- features/support/*'                   \
	  -f 'P *.elc' -f '+ */' -f '+ *.el' -f '- *' \
	  --prune-empty-dirs --delete-excluded        \
	  packages/ $(PKG_INSTALL_DIR)/
	@
	@echo Byte-compiling packages
	emacs -q -batch --load "init.el" \
	  --eval '(byte-recompile-directory "$(PKG_INSTALL_DIR)" 0)'

	chmod -R a-w $(PKG_INSTALL_DIR)

bc-emacsd:
	emacs -q --batch --load "init.el" \
	  --load "bin/byte-compile.el"    \
	  --eval '(byte-recompile-directory-non-recursive "./")'

clean: bc-clean
bc-clean:
	find $(PKG_INSTALL_DIR) -name '*.elc' -delete
	$(RM) *.elc

distclean: bc-distclean
bc-distclean:
	$(RM) $(PKG_INSTALL_DIR)


# * pydoc-info

all: pydoc-info

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


# * Yasnippet

all: yasnippet
yasnippet:
	emacs -q --batch --load init.el --eval '(yas-recompile-all)'



# * Postamble

# Local Variables:
#  eval: (outline-minor-mode 1)
# End:
