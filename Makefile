VERSION?=$(shell grep '^[Vv]ersion:' citeproc.cabal | awk '{print $$2;}')

.PHONY: locales test bench ghcid repl clean update-locales update-test-suite update-locales-upstream test-diff

test:
	cabal test --jobs=1 --test-show-details=streaming --test-options="$(PATTERN)" --ghc-option=-Wall | tee spec.log

test-diff: test
	cp spec.failed spec.failed.prev || exit 0
	grep '\[FAIL\|\[ERROR' spec.log > spec.failed
	diff -u spec.failed.prev spec.failed | grep '^[+-]' | tee spec.failed.diff

clean:
	cabal clean

repl:
	cabal repl --repl-options=-interactive-print=Text.Pretty.Simple.pPrint --build-depends=pretty-simple

ghcid:
	ghcid --command "cabal repl"

bench:
	cabal bench

locales-upstream:
	git clone https://github.com/citation-style-language/locales $@

update-locales-upstream: locales-upstream
	cd $< && git pull

update-locales: update-locales-upstream
	cp locales-upstream/locales.json locales/
	for f in locales-upstream/*.xml; do cp -v $$f locales/$${f#locales-upstream/locales-} ; done

locales/%.xml: locales-upstream/locales-%.xml
	cp $< $@

man/citeproc.1: man/citeproc.1.md
	pandoc $< -s -Vfooter="citeproc $(VERSION)" -o $@

test-suite:
	git clone https://github.com/citation-style-language/test-suite

update-test-suite: test-suite
	cd $< && git pull
	cp $</processor-tests/humans/*.txt test/csl/

test-suite-upstream:
	 git clone https://github.com/citation-style-language/test-suite.git
