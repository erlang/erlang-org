.PHONY: patches documentation setup build update serve check algolia

build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" -o -name "*.js" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/

vendor/bundle:
	bundler install

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

documentation: otp_versions.table docs
	_scripts/download-docs.sh $<

_scripts/_build/default/bin/erlang-org:
	$(MAKE) -C _scripts

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

_eeps: _scripts/_build/default/bin/erlang-org eeps
	$< format-eeps $@ _clones/eep/eeps/eep-0000.html eeps/*.md

_patches assets/js assets/webfonts _clones docs:
	mkdir -p $@

patches: _data/releases.json

_data/releases.json: _scripts/_build/default/bin/erlang-org otp_versions.table _patches
	$< create-releases otp_versions.table _data/releases.json _patches/

update:
	npm update

setup: vendor/bundle setup_npm _data/releases.json documentation _eeps faq

serve: setup
	bundle exec jekyll serve --incremental --trace --livereload

check: setup
	_scripts/check.sh

lint:
	yamllint -f standard .
	npm run shellcheck

algolia:
	_scripts/run_algolia_scraper.sh
