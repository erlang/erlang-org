.PHONY: setup clean build update serve test algolia setup_gems setup_npm format-eeps patches assets/img/favicon.ico build-docs otp-headers

## For netlify the BUNDLE_PATH is different so we need to check it
BUNDLE_PATH?=vendor/bundle

build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" -o -name "*.js" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/

netlify: clean
	$(MAKE) -j $(shell nproc) --debug=basic BUNDLE_PATH=/opt/build/cache/bundle JEKYLL_ENV=production

clean:
	rm -rf _patches docs _eeps faq _clones eeps assets/js LATEST_MAJOR_VSN

$(BUNDLE_PATH):
	bundler install --jobs 4 --retry 3 --path $(BUNDLE_PATH)

setup_gems: $(BUNDLE_PATH)

node_modules: package-lock.json | assets/js
	npm install
	npm run build

assets/js/bootstrap: | assets/js
	ln -s ../../node_modules/bootstrap assets/js/bootstrap
assets/js/fontawesome-free: | assets/js
	ln -s ../../node_modules/@fortawesome/fontawesome-free/js assets/js/fontawesome-free
assets/webfonts/fontawesome-free: | assets/webfonts
	ln -s ../../node_modules/@fortawesome/fontawesome-free/webfonts assets/webfonts/fontawesome-free
assets/js/prismjs: | assets/js
	ln -s ../../node_modules/prismjs assets/js/prismjs

assets/js/doc-search.bundle.js: | assets/js
	npm run build

## Use imagemagic to create a favico from the svg logo
assets/img/favicon.ico: assets/img/erlang-logo.svg
	convert assets/img/erlang-logo.svg -delete 1--1 \( -clone 0 -resize 16x16\! \) \( -clone 0 -resize 48x48\! \) \( -clone 0 -resize 96x96\! \) \( -clone 0 -resize 144x144\! \) -delete 0 -background none assets/img/favicon.ico

setup_npm: node_modules assets/js/doc-search.bundle.js | assets/js/bootstrap assets/js/fontawesome-free assets/webfonts/fontawesome-free assets/js/prismjs

otp_versions.table:
	curl https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table > $@

_scripts/_build/default/bin/erlang-org: $(wildcard _scripts/src/*.erl) _scripts/rebar.config
	$(MAKE) -C _scripts

_clones/eep: | _clones
	git clone https://github.com/erlang/eep $@
	cd $@ && ./build.pl

_clones/faq: | _clones
	git clone https://github.com/matthiasl/Erlang-FAQ $@

faq: _clones/faq
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	ls -la $@
	FAQ_HASH=$(shell (cat .tool-versions && cd $< && git rev-parse --short HEAD) | sha256sum | awk '{print $$1}') && \
	if [ ! -f $@/$${FAQ_HASH} ]; then \
	  rm -rf $@/* && \
	  (cd $< && LC_ALL="en_US-UTF8" make && make install FAQ_ROOT=../../$@) && \
	  touch $@/$${FAQ_HASH}; \
	fi
	ls -la $@

eeps: _clones/eep
	-mkdir $@
	cp -r $(wildcard _clones/eep/eeps/*.md) $(wildcard _clones/eep/eeps/*.png) $(wildcard _clones/eep/eeps/*.diff) $@/

EEPS_DEPS=_scripts/src/format-eeps.erl _scripts/src/gh.erl
EEPS_HASH=$(shell cat $(EEPS_DEPS) | sha256sum - | awk '{print $$1}')
_eeps: _clones/eep $(EEPS_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ ! -f $@/$(shell cd $< && git rev-parse --short HEAD)-$(EEPS_HASH) ]; then \
	  $(MAKE) format-eeps; \
	fi

format-eeps: _scripts/_build/default/bin/erlang-org _clones/eep
	rm -rf _eeps/*
	$< format-eeps _eeps _clones/eep/eeps/eep-0000.html _clones/eep/eeps/*.md
	touch _eeps/$(shell cd _clones/eep && git rev-parse --short HEAD)-$(EEPS_HASH)

LATEST_MAJOR_VSN: otp_versions.table
	@set -e ;\
	MAJOR_VSNs=$$(grep "OTP-[0-9]\+\.0 " $< \
	| awk '{print $1}' \
	| sed 's/OTP-\(.*\)/\1/g' \
	| sed 's/^\([0-9]\+\).*/\1/g') ;\
	LATEST_MAJOR_VSN=$$(echo "$$MAJOR_VSNs" | tr ' ' '\n' | sort -n | tail -1) ;\
	echo $$LATEST_MAJOR_VSN > $@
	

docs: otp_versions.table _scripts/download-docs.sh _scripts/otp_flatten_docs _scripts/otp_flatten_ex_docs _scripts/otp_doc_sitemap.sh LATEST_MAJOR_VSN _scripts/otp_add_headers.sh
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ "$(JEKYLL_ENV)" != "production" ]; then _scripts/download-docs.sh $<; fi
	@touch docs

PATCHES_DEPS=otp_versions.table _scripts/src/create-releases.erl _scripts/src/otp_readme.erl _scripts/src/gh.erl
PATCHES_HASH=$(shell cat $(PATCHES_DEPS) | sha256sum - | awk '{print $$1}')
_patches: $(PATCHES_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ ! -f _patches/$(PATCHES_HASH) ]; then $(MAKE) patches; fi

_clones:
	mkdir -p $@
	echo "erlang 26.2.5.9" > _clones/.tool-versions

assets/js assets/webfonts:
	mkdir -p $@

patches: _scripts/_build/default/bin/erlang-org otp_versions.table
	-mkdir _patches
	rm -f _patches/*
	$< create-releases otp_versions.table _patches/releases.json _patches/
	touch _patches/$(PATCHES_HASH)

update:
	npm update

otp-headers: docs _redirects _scripts/otp_add_headers.sh LATEST_MAJOR_VSN
	if [ ! -f "$@" ] || [ ! $$(cat "$@") = $$(tar c "$<" | md5sum | awk '{print $$1}') ]; then \
		_scripts/otp_add_headers.sh "$<"; \
	fi;
	tar c $< | md5sum | awk '{print $$1}' > $@

build-docs: docs otp-headers

_redirects: _redirects.in _scripts/redirects.sh docs
	cp _redirects.in "$@"
	_scripts/redirects.sh >> "$@"

setup: setup_gems setup_npm _patches docs _eeps eeps faq _redirects otp-headers

serve: setup
	bundle exec jekyll serve --future --incremental --trace --livereload --host 0.0.0.0

test:
	DEPLOY=true $(MAKE) setup
	yamllint -f standard .
	npm run shellcheck
	_scripts/check.sh

algolia:
	_scripts/run_algolia_scraper.sh
