
build: setup
	bundler exec jekyll build

vendor/bundle:
	bundler install --path vendor/bundle

node_modules: package-lock.json
	npm install

update:
	npm update

setup: vendor/bundle node_modules

serve: setup
	bundle exec jekyll serve --trace --livereload
