
JS=_site/assets/js
NM=_site/node_modules
build: setup
	bundler exec jekyll build
	npx purgecss --css _site/assets/css/*.css --safelist "token" --content `find _site -name "*.html" | grep -v _site/doc/ | grep -v _site/docs/`  -o _site/assets/css/
	# rm -rf $(JS)/{bootstrap,@fortawesome,prismjs}
	# mkdir -p $(JS)/bootstrap/dist/js/ $(JS)/prismjs/components $(JS)/@fortawesome/fontawesome-free/
	# cp $(NM)/bootstrap/dist/js/bootstrap.*min.js* $(JS)/bootstrap/dist/js/
	# cp $(NM)/prismjs/prism.js $(JS)/prismjs/
	# cp $(NM)/prismjs/components/prism-erlang.js $(JS)/prismjs/components/
	# cp -r $(NM)/@fortawesome/fontawesome-free/webfonts $(JS)/@fortawesome/fontawesome-free/
	# rm -rf $(NM)/

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

setup_npm: node_modules assets/js
	if [ ! -L assets/js/bootstrap ]; then ln -s ../../node_modules/bootstrap assets/js/bootstrap; fi
	if [ ! -L assets/js/@fortawesome ]; then ln -s ../../node_modules/bootstrap assets/js/@fortawesome; fi
	if [ ! -L assets/js/prismjs ]; then ln -s ../../node_modules/bootstrap assets/js/prismjs; fi

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

setup: vendor/bundle setup_npm _data/releases.json docs

serve: setup
	bundle exec jekyll serve --trace --livereload
