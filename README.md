# Erlang.org

This is the repository for the erlang.org website.

We use [ruby] /w [jekyll], [nodejs] /w [bootstrap 5] and Erlang to build this website.

To launch a local copy, install the correct [prerequisites](#Prerequisites) and do `make serve` and navigate to http://localhost:4000.

You can view the latest prototype here: https://garazdawi.github.io/jekyll-test

[ruby]: https://ruby.org
[jekyll]: https://jekyllrb.com/
[nodejs]: https://nodejs.org
[bootstrap 5]: https://getbootstrap.com/docs/5.0/

## Makefile

The makefile supports three targets

* build (default) - depeds on setup
  * Builds the entire site under `_site` for exporting
* serve - depends on setup
  * start jekyll to serve the erlang.org site at http://localhost:4000
* setup
  * Download and generate all [Auto-generated Content](#Auto-generated-content).

## Adding content

There are three major collections that you can add new items to: [News], [Blog] and [Release]. Each of these are markdown files found in _news, _posts and _releases respectively.

There is a README file in each of those folders that describe the mandatory front matter for each item.

Links on the documentation page are added in [_data/doc-links.yaml].

[News]: /_news/README.md
[Blog]: /_posts/README.md
[Release]: /_releases/README.md

## Auto-generated content

When doing `make setup` the auto-generated content is created. We auto-generate this:

### EEPs

This is placed under `_eeps`.

Clone https://github.com/erlang/eep then parse using [_scripts/src/format-eeps.erl]. We do not use
the perl markdown formatter for EEPs as the html produced does not look very nice.

### FAQ

This is placed under `faq`.

Clone https://github.com/matthiasl/Erlang-FAQ and then build it.

### Patches

This is placed under `_data/release.json` and `_patches`.

We fetch the latest [otp_versions.table] and from there use the [Github API](https://docs.github.com/en/rest)
and the erlang.org rsync to fetch information about each patch released since OTP-17.0.

The files in `_patches` and `_data/release.json` contain a lot of duplicate information. We could have kept the
`_data/release.json` as the only place to keep the data, but we didn't as doing lookups in it turned out to
be too slow for jekyll.

[otp_versions.table]: https://github.com/erlang/otp/blob/master/otp_versions.table

### Documentation

This is placed under `docs`.

The latest documentation for each release since OTP-17 is downloaded+flattened and put into the `docs` folder.
The documentation is not built from scratch but rather fetched from github releases or erlang.org.

## Prerequisites

You need to have the following tools installed to build the erlang.org site:

* GNU make 4.1 or later
* ruby 2.6.5 or later
* bundler 1.16 or later
* nodejs 14 or later
* erlang 24 or later

Most likely earlier versions of these tools will work, but they have not been tested.

TODO: Create docker image to make sure we have the correct versions

## Development



### Layout

erlang.org uses a combination of [CSS Grid] layout and [Bootstrap 5
Grid] layout. The goal is to use CSS Grid for all responsive layouts
and then use Bootstrap Grid for all the non-responsive things.

Using CSS Grid to do the responsive layout instead of Bootstrap
removes a lot of extra divs and `order` classes that are needed
otherwise. However, it removes the layout from the html, so it
sometimes becomes less obvious what is going on.

[CSS Grid]: https://css-tricks.com/snippets/css/complete-guide-grid/
[Bootstrap 5 Grid]: https://getbootstrap.com/docs/5.0/layout/grid/

The HTML for a normal page looks something like this:

```html
<body>
    <header class="container header">
        <nav></nav>
    </header>
    <div class="container body">
      <aside class="sidebar"></aside>
      <main class="main">
          <div class="top"></div>
          <div class="content"></div>
      </main>
    </div>
    <footer class="container footer"></footer>
</body>
```

In the above the `container` class is part of bootstrap and is used for
styling and then we use CSS grid to place the content of the `body` and
`main` classes responsively:

```scss
@include media-breakpoint-up(lg) {
    .body {
        display: grid;
        /* 2 columns on > lg screens */
        grid-template-columns: 1fr auto;
    }
    .main {
        display: grid;
    }
}
@include media-breakpoint-down(lg) {
    .body {
        display: grid;
        /* Hide the sidebar on small screens */
        .sidebar {
            display: none;
        }
    }
    .main {
        display: grid;
    }
}
```

### Manipulating CSS

Bootstrap 5 comes with a lot of css entities built in. You should have a look around in the [bootstrap docs] to see what you can use.

If you want to change the color of a specific component there is a list of the sass variable that you need to change in the specific page. For example if you want to change the font-size in badges you can lookup the variable here: https://getbootstrap.com/docs/5.0/components/badge/#sass. And then set `$badge-font-size: 0.80em` in [_variables.scss](_sass/_variables.scss]).

A full list of all the variables can be found in `node_modules/bootstrap/scss/_variables.scss`.

You can of course also create your own styles, but we try to stay with the bootstrap styles as much as possible.

[bootstrap docs]: https://getbootstrap.com/docs/5.0/

## Architecture

## TODO

* Integrate docsearch.algolia.com search into erlang.org/doc
* Add plausable.io tracking
* Use [BackstopJS](https://css-tricks.com/automating-css-regression-testing/) for regression testing 
* Fix github ratelimiting issue.
  * Download all READMEs from rsync erlang.org
  * Update the github sync script to work better?
    * https://github.com/erlang/otp/blob/master/.github/workflows/sync-github-releases.yaml

### Notes
* `/docs`
  * Fix responsivness
  * Other sections? Learning/Developing/References
* `/community`
  * Beam Languages
  * Other projects
  * erlang companies.org
* `/downloads`
  * Link to blogpost about release
* `/patches`
  * Javascript collapse of sidebar

## Things that have been removed

* The course
* The events
* Documentation versions

## Ideas

