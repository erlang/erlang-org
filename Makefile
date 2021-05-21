
build: setup
	bundler exec jekyll build

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

_data:
	mkdir _data
_data/releases.json: _data
	curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/erlang/otp/releases > $@
update:
	npm update

setup: vendor/bundle node_modules _data/releases.json

serve: setup
	bundle exec jekyll serve --trace --livereload
