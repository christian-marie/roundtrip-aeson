SOURCES=$(shell find lib tests -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

STYLISHHASKELL=$(shell which stylish-haskell 2>/dev/null)
STYLISH=$(if $(STYLISHHASKELL),$(STYLISHHASKELL),/bin/false)

all: tags

.PHONY: all test clean

tags: $(SOURCES)
	@if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	@$(CTAGS) $^ > tags $(REDIRECT)

clean:
	@/bin/echo -e "CLEAN"
	@cabal clean >/dev/null
	@rm -f tags

lint: $(SOURCES)
	for i in $^; do hlint $$i; done

test:
	@/bin/echo -e "TEST"
	cabal test

build:
	cabal build

format: $(SOURCES)
	for i in $^; do stylish-haskell -i $$i; done
