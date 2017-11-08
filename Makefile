TESTDIR=$(shell ./lsprj)
SOURCEJS=$(addsuffix /bin/*/all.js,$(TESTDIR))
CURRDIR=$(shell pwd)
PRJNAMES=$(shell echo $(subst $(CURRDIR),,$(SOURCEJS)) | sed -e "s/ /\n/g" | sed -e "s/\/\([^\/]*\).*/\1/g")
BUILDS=$(addsuffix /all.js,$(addprefix docs/,$(PRJNAMES)))
HTML=$(addsuffix /index.html,$(addprefix docs/,$(PRJNAMES)))

func = $(shell echo $(subst $(CURRDIR),,$(1)) | sed -e "s/ /\n/g" | sed -e "s/\/\([^\/]*\).*/\1/g")

all: $(BUILDS) $(HTML)

yes:
	echo PRJNAMES: $(PRJNAMES)
	echo BUILDS: $(BUILDS)
	echo $(call func,$(wildcard $(TESTDIR)/bin/*/all.js))

$(BUILDS): $(shell $(CURRDIR)/getsrchs $@)
	$(if $(shell find `$(CURRDIR)/prname $@`), $(shell mkdir docs/`$(CURRDIR)/prname $@`),)
	cp $(shell $(CURRDIR)/getsrchs $@) $@

$(HTML): template.html
	cp template.html $@

$(TESTDIR)/bin/*/all.js: $(call func,$@)/src/Lib.hs
	cd $(call func,$@)
	stack build

clean:
	rm -f ./docs/ghcjs-test/*.js

.PHONY: all clean
