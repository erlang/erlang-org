# Erlang.org

This is the repository for the erlang.org website.

We use [ruby] /w [jekyll] and [nodejs] /w [bootstrap 5] to build this website.

To launch a local copy, install the correct [prerequisites](#Prerequisites) and do `make serve` and navigate to http://localhost:8080.

You can view the latest prototype here: https://garazdawi.github.io/jekyll-test

[ruby]: https://ruby.org
[jekyll]: https://jekyllrb.com/
[nodejs]: https://nodejs.org
[bootstrap 5]: https://getbootstrap.com/docs/5.0/

## Adding content

## Prerequisites

You need to have the following tools installed to build the erlang.org site:

* GNU make 4.1 or later
* ruby 2.6 or later
* bundler 1.16 or later
* nodejs 14 or later

Most likely earlier versions of these tools will work, but they have not been tested.

TODO: Create docker image to make sure we have the correct versions

## Manipulating CSS

Bootstrap 5 comes with a lot of css entities built in. You should have a look around in the [bootstrap docs] to see what you can use.

If you want to change the color of a specific component there is a list of the sass variable that you need to change in the specific page. For example if you want to change the font-size in badges you can lookup the variable here: https://getbootstrap.com/docs/5.0/components/badge/#sass. And then set `$badge-font-size: 0.80em` in [_variables.scss](_sass/_variables.scss]).

A full list of all the variables can be found in `node_modules/bootstrap/scss/_variables.scss`.

You can of course also create your own styles, but we try to stay with the bootstrap styles as much as possible.

[bootstrap docs]: https://getbootstrap.com/docs/5.0/

## Architecture