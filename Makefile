.PHONY: setup clean build update serve test algolia setup_gems setup_npm format-eeps patches assets/img/favicon.ico build-docs otp-headers versions

## For netlify the BUNDLE_PATH is different so we need to check it
BUNDLE_PATH?=vendor/bundle

build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" -o -name "*.js" | grep -v _site/doc/ | grep -v _site/docs/` --safelist '/^alg-/' '/^otpv-/' '/^sev-/' -o _site/assets/css/

netlify: clean
	$(MAKE) -j $(shell nproc 2>/dev/null || sysctl -n hw.ncpu) --debug=basic BUNDLE_PATH=/opt/build/cache/bundle JEKYLL_ENV=production

clean:
	rm -rf _patches _patches.new _versions _versions.new assets/otp-versions.json assets/otp-tickets.json docs _eeps faq _clones/eep _clones/faq eeps assets/js _redirects LATEST_MAJOR_VSN

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

_clones/eep:
	git clone https://github.com/erlang/eep $@
	cd $@ && ./build.pl

_clones/faq:
	git clone https://github.com/matthiasl/Erlang-FAQ $@

faq: _clones/faq
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	ls -la $@
	FAQ_HASH=$(shell (cat .tool-versions && cd $< && git rev-parse --short HEAD) | shasum -a 256 | awk '{print $$1}') && \
	if [ ! -f $@/$${FAQ_HASH} ]; then \
	  rm -rf $@/* && \
	  (cd $< && LC_ALL="en_US-UTF8" make && make install FAQ_ROOT=../../$@) && \
	  touch $@/$${FAQ_HASH}; \
	fi
	ls -la $@

eeps: _clones/eep
	-mkdir $@
	cp -r $(wildcard _clones/eep/eeps/*.md) $(wildcard _clones/eep/eeps/*.png) $(wildcard _clones/eep/eeps/*.diff) $@/

EEPS_DEPS=_scripts/src/format-eeps.erl _scripts/src/eep-news.erl _scripts/src/gh.erl
EEPS_HASH=$(shell cat $(EEPS_DEPS) | shasum -a 256 | awk '{print $$1}')
## Trusts the cache in production like docs, _patches and _versions: a deploy
## preview has no CI run to refresh the branch first, and regenerating needs
## rebar3 and Erlang, which that build does not have. Note that gh.erl is a
## dependency here as well as of _patches and _versions, so touching it
## invalidates all three.
_eeps: _clones/eep $(EEPS_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ "$(JEKYLL_ENV)" != "production" ] && \
	   [ ! -f $@/$(shell cd $< && git rev-parse --short HEAD)-$(EEPS_HASH) ]; then \
	  $(MAKE) format-eeps; \
	fi

format-eeps: _scripts/_build/default/bin/erlang-org _clones/eep
	rm -rf _eeps/*
	$< format-eeps _eeps _clones/eep/eeps/eep-0000.html _clones/eep/eeps/*.md
	rm -f _news/eep-*.md
	$< eep-news _news _clones/eep
	touch _eeps/$(shell cd _clones/eep && git rev-parse --short HEAD)-$(EEPS_HASH)

LATEST_MAJOR_VSN: otp_versions.table
	@set -e ;\
	MAJOR_VSNs=$$(grep "OTP-[0-9][0-9]*\.0 " $< \
	| awk '{print $1}' \
	| sed 's/OTP-\(.*\)/\1/g' \
	| sed 's/^\([0-9][0-9]*\).*/\1/g') ;\
	LATEST_MAJOR_VSN=$$(echo "$$MAJOR_VSNs" | tr ' ' '\n' | sort -n | tail -1) ;\
	echo $$LATEST_MAJOR_VSN > $@
	

docs: otp_versions.table _scripts/download-docs.sh _scripts/otp_flatten_docs \
	_scripts/otp_flatten_ex_docs _scripts/otp_doc_sitemap.sh _scripts/otp_add_headers.sh \
	_redirects.in _scripts/redirects.sh LATEST_MAJOR_VSN
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ "$(JEKYLL_ENV)" != "production" ]; then _scripts/download-docs.sh $^; fi
	@touch docs

VERSIONS_DEPS=otp_versions.table _scripts/src/create-versions.erl _scripts/src/gh.erl
VERSIONS_HASH=$(shell cat $(VERSIONS_DEPS) | shasum -a 256 | awk '{print $$1}')
## Generating this needs Erlang and a few hundred GitHub API calls, neither of
## which the Netlify build has, so the result is cached in the _versions branch
## and cloned back in. Falls back to generating locally while the branch does
## not exist yet.
_versions: $(VERSIONS_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@ || mkdir -p $@; fi
	if [ "$(JEKYLL_ENV)" != "production" ] && [ ! -f _versions/$(VERSIONS_HASH) ]; then \
	  $(MAKE) versions; \
	fi

## Copied rather than symlinked: jekyll follows a symlinked directory but
## copies a symlinked file as the link itself, which dangles in _site.
assets/otp-versions.json: _versions
	cp _versions/otp-versions.json $@

## The ticket index belongs to the patches cache, so the two caches stay
## independent of one another and the page joins them at run time.
assets/otp-tickets.json: _patches
	if [ -f _patches/tickets.json ]; then cp _patches/tickets.json $@; else echo '{}' > $@; fi

PATCHES_DEPS=otp_versions.table _scripts/src/create-releases.erl _scripts/src/otp_readme.erl _scripts/src/gh.erl
PATCHES_HASH=$(shell cat $(PATCHES_DEPS) | shasum -a 256 | awk '{print $$1}')
_patches: $(PATCHES_DEPS)
	if [ ! -d $@ ]; then git clone --single-branch -b $@ https://github.com/erlang/erlang-org $@; fi
	if [ "$(JEKYLL_ENV)" != "production" ] && [ ! -f _patches/$(PATCHES_HASH) ]; then \
	  $(MAKE) patches; \
	fi

assets/js assets/webfonts:
	mkdir -p $@

versions: _scripts/_build/default/bin/erlang-org otp_versions.table
	rm -rf _versions.new && mkdir _versions.new
	$< create-versions otp_versions.table _versions.new/otp-versions.json
	touch _versions.new/$(VERSIONS_HASH)
	-mkdir _versions
	rm -f _versions/*
	mv _versions.new/* _versions/ && rmdir _versions.new

patches: _scripts/_build/default/bin/erlang-org otp_versions.table
	rm -rf _patches.new && mkdir _patches.new
	$< create-releases otp_versions.table _patches.new/releases.json _patches.new/
	touch _patches.new/$(PATCHES_HASH)
	-mkdir _patches
	rm -f _patches/*
	mv _patches.new/* _patches/ && rmdir _patches.new

update:
	npm update

_redirects: _redirects.in _scripts/redirects.sh docs
	cp _redirects.in "$@"
	_scripts/redirects.sh >> "$@"

setup: setup_gems setup_npm _patches assets/otp-versions.json assets/otp-tickets.json docs _eeps eeps faq _redirects otp-headers

serve: setup
	bundle exec jekyll serve --future --incremental --trace --livereload --host 0.0.0.0

test:
	DEPLOY=true $(MAKE) setup
	yamllint -f standard .
	npm run shellcheck
	_scripts/check.sh
	_scripts/check_algolia_search.sh
	_scripts/check_redirects.sh

algolia:
	_scripts/run_algolia_scraper.sh
