
build: setup
	$(MAKE) build-no-setup

build-no-setup:
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" -o -name "*.js" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

setup_npm: node_modules | assets/js assets/webfonts
	if [ ! -L assets/js/bootstrap ]; then \
		ln -s ../../node_modules/bootstrap assets/js/bootstrap;\
	fi
	if [ ! -L assets/js/fontawesome-free ]; then \
		ln -s ../../node_modules/@fortawesome/fontawesome-free/js assets/js/fontawesome-free;\
	fi
	if [ ! -L assets/webfonts/fontawesome-free ]; then \
		ln -s ../../node_modules/@fortawesome/fontawesome-free/webfonts assets/webfonts/fontawesome-free;\
	fi
	if [ ! -L assets/js/prismjs ]; then \
		ln -s ../../node_modules/prismjs assets/js/prismjs;\
	fi

otp_versions.table:
	curl https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table > $@

docs: otp_versions.table
	-mkdir $@
	_scripts/download-docs.sh $<

.PHONY: _scripts patches

_scripts:
	$(MAKE) -C $@

_clones/eep: | _clones
	git clone https://github.com/erlang/eep $@
	cd $@ && ./build.pl

_clones/faq: | _clones
	-git clone https://github.com/matthiasl/Erlang-FAQ $@
	cd $@ && LC_ALL="en_US-UTF8" make

faq: _clones/faq
	cd $< && make install FAQ_ROOT=../../$@

eeps: _clones/eep
	-mkdir $@
	cp -r $(wildcard _clones/eep/eeps/*.md) $(wildcard _clones/eep/eeps/*.png) $(wildcard _clones/eep/eeps/*.diff) $@/

_eeps: _scripts eeps
	_scripts/_build/default/bin/erlang-org format-eeps $@ _clones/eep/eeps/eep-0000.html eeps/*.md

_patches assets/js assets/webfonts _clones:
	mkdir -p $@

patches:
	_scripts/_build/default/bin/erlang-org create-releases otp_versions.table _data/releases.json _patches/
_data/releases.json: _scripts otp_versions.table | _patches
	 make patches

update:
	npm update

setup: vendor/bundle setup_npm _data/releases.json docs _eeps faq

serve: setup
	bundle exec jekyll serve --incremental --trace --livereload

check: setup
	_scripts/check.sh

algolia:
	_scripts/run_algolia_scraper.sh
