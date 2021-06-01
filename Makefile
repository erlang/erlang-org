
build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --content `find _site -name "*.html" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/
	rm -f _site/assets/js/{bootstrap,@fortawesome,prismjs}
	mkdir -p _site/assets/js/bootstrap/dist/js/ _site/assets/js/prismjs/components _site/assets/js/@fortawesome/fontawesome-free/
	cp _site/node_modules/bootstrap/dist/js/bootstrap.*min.js* _site/assets/js/bootstrap/dist/js/
	cp _site/node_modules/prismjs/prism.js _site/assets/js/prismjs/
	cp _site/node_modules/prismjs/components/prism-erlang.js _site/assets/js/prismjs/components/
	cp -r _site/node_modules/@fortawesome/fontawesome-free/webfonts _site/assets/js/@fortawesome/fontawesome-free/
	rm -rf _site/node_modules/

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json assets/js
	npm install
	ln -s ../../node_modules/bootstrap assets/js/bootstrap
	ln -s ../../node_modules/@fortawesome assets/js/@fortawesome
	ln -s ../../node_modules/prismjs assets/js/prismjs

otp_versions.table:
	curl https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table > $@

docs: otp_versions.table
	-mkdir $@
	_scripts/download-docs.sh

_releases assets/js:
	mkdir -p $@

_data/releases.json: _releases node_modules _data/download.ts
	npx ts-node _data/download.ts _data/releases.json _releases/
update:
	npm update

setup: vendor/bundle node_modules _data/releases.json docs

serve: setup
	bundle exec jekyll serve --trace --livereload
