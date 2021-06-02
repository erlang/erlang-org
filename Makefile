
build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --safelist "token" --content `find _site -name "*.html" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

setup_npm: node_modules assets/js
	if [ ! -L assets/js/bootstrap ]; then \
		ln -s ../../node_modules/bootstrap assets/js/bootstrap;\
	fi
	if [ ! -L assets/js/@fortawesome ]; then \
		ln -s ../../node_modules/@fortawesome assets/js/@fortawesome;\
	fi
	if [ ! -L assets/js/prismjs ]; then \
		ln -s ../../node_modules/prismjs assets/js/prismjs;\
	fi

otp_versions.table:
	curl https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table > $@

docs: otp_versions.table
	-mkdir $@
	_scripts/download-docs.sh

eep:
	git clone https://github.com/erlang/eep
	(cd eep && ./build.pl)

_eeps: eep
	mkdir $@
	cp -r $(wildcard eep/eeps/*.html) $(wildcard eep/eeps/*.png) $@/
	sed -i '1s;^;---\nlayout: eep\n---\n{% raw %}\n;' $@/*.html
	for i in ls $@/*.html; do echo "\n{% endraw %}" >> $$i; done
eeps.html: _eeps
	if [ ! -L eeps.html ]; then \
		ln -s _eeps/eep-0000.html eeps.html; \
	fi

_releases assets/js:
	mkdir -p $@

_data/releases.json: _releases node_modules _data/download.ts
	npx ts-node _data/download.ts _data/releases.json _releases/
update:
	npm update

setup: vendor/bundle setup_npm _data/releases.json docs eeps.html

serve: setup
	bundle exec jekyll serve --trace --livereload
