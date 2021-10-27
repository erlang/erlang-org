.PHONY: setup clean build update serve test algolia setup_gems setup_npm format-eeps patches

## For netlify the BUNDLE_PATH is different so we need to check it
BUNDLE_PATH?=vendor/bundle

build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" -o -name "*.js" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/

netlify: clean
	$(MAKE) -j $(shell nproc) BUNDLE_PATH=/opt/build/cache/bundle JEKYLL_ENV=production

clean:
	rm -rf _patches docs _eeps faq _clones eeps

$(BUNDLE_PATH):
	bundler install --jobs 4 --retry 3 --path $(BUNDLE_PATH)

setup_gems: $(BUNDLE_PATH)

node_modules: package-lock.json
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

setup_npm: node_modules | assets/js/bootstrap assets/js/fontawesome-free assets/webfonts/fontawesome-free assets/js/prismjs

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
	if [ ! -f $@/$(shell cd $< && git rev-parse --short HEAD) ]; then \
	  rm -rf $@/* && \
	  (cd $< && LC_ALL="en_US-UTF8" make && make install FAQ_ROOT=../../$@) && \
	  touch $@/$(shell cd $< && git rev-parse --short HEAD); \
	fi

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

docs: otp_versions.table _scripts/otp_flatten_docs _scripts/otp_doc_sitemap.sh assets/doc-search.tsx
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	_scripts/download-docs.sh $<

PATCHES_DEPS=otp_versions.table _scripts/src/create-releases.erl _scripts/src/otp_readme.erl _scripts/src/gh.erl
PATCHES_HASH=$(shell cat $(PATCHES_DEPS) | sha256sum - | awk '{print $$1}')
_patches: $(PATCHES_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ ! -f _patches/$(PATCHES_HASH) ]; then $(MAKE) patches; fi

assets/js assets/webfonts _clones:
	mkdir -p $@

patches: _scripts/_build/default/bin/erlang-org otp_versions.table
	rm -f _patches/*
	$< create-releases otp_versions.table _patches/releases.json _patches/
	touch _patches/$(PATCHES_HASH)

update:
	npm update

setup: setup_gems setup_npm _patches docs _eeps eeps faq

serve: setup
	bundle exec jekyll serve --incremental --trace --livereload --host 0.0.0.0

test:
	DEPLOY=true $(MAKE) setup
	yamllint -f standard .
	npm run shellcheck
	_scripts/check.sh

algolia:
	_scripts/run_algolia_scraper.sh
