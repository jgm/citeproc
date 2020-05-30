LOCALE_DATA=$(patsubst locales-upstream/locales-%.xml,data/locales/%.xml, $(wildcard locales-upstream/*.xml)) data/locales/locales.json

.PHONY: test-suite locales test bench ghcid repl clean update-locales update-test-suite update-locales-upstream test-diff

test:
	cabal test --jobs=1 --test-show-details=streaming --test-options="$(PATTERN)" | tee spec.log

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

update-locales: update-locales-upstream $(LOCALE_DATA)

data/locales/%.xml: locales-upstream/locales-%.xml
	cp $< $@

data/locales/locales.json: locales-upstream/locales.json
	cp $< $@

update-test-suite: test-suite-upstream
	cd $< && git pull
	cp $</processor-tests/humans/*.txt test/csl/

test-suite-upstream:
	 git clone https://github.com/citation-style-language/test-suite.git
