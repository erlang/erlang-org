
build: setup
	bundler exec jekyll build

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

_releases:
	mkdir -p $@
_data/releases.json: _releases node_modules
	npx ts-node _data/download.ts _data/releases.json _releases/
update:
	npm update

setup: vendor/bundle node_modules _data/releases.json

serve: setup
	bundle exec jekyll serve --trace --livereload
