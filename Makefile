TESTDIR=$(shell cd ghcjs-test/ && stack path --local-install-root)
SOURCEJS=$(wildcard $(TESTDIR)/bin/ghcjs-test-exe.jsexe/all.js)
BUILDS=$(addprefix docs/ghcjs-test/,$(notdir $(SOURCEJS)))

all: $(BUILDS)

yes:
	echo $(TESTDIR)/bin/ghcjs-test-exe.jsexe/*.js | less

docs/ghcjs-test/%.js: $(TESTDIR)/bin/ghcjs-test-exe.jsexe/%.js
	cp $< $@

$(TESTDIR)/bin/ghcjs-test-exe.jsexe/all.js: ghcjs-test/src/Lib.hs ghcjs-test/app/Main.hs
	cd ghcjs-test/; stack build

clean:
	rm -f ./docs/ghcjs-test/*.js

.PHONY: all clean
