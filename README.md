# Erlang.org

[![Netlify Status](https://api.netlify.com/api/v1/badges/dedfbd28-2e3c-4c37-a08e-7b7a580eb43a/deploy-status)](https://app.netlify.com/sites/erlang-org-garazdawi/deploys)

This is the repository for the erlang.org website.

We use [ruby] w/ [jekyll], [nodejs] w/ [bootstrap 5], and Erlang to build this website.

To launch a local copy, install the correct [prerequisites](#Prerequisites) and do `make serve` and navigate to http://localhost:4000.

You can view the deployed version here: <https://www.erlang.org>

[ruby]: https://www.ruby-lang.org/en/
[jekyll]: https://jekyllrb.com/
[nodejs]: https://nodejs.org
[bootstrap 5]: https://getbootstrap.com/docs/5.0/

## Makefile

The makefile supports these targets

* build (default) - depends on setup
  * Builds the entire site under `_site` for exporting
* serve - depends on setup
  * start jekyll to serve the erlang.org site at http://localhost:4000
* setup
  * Download and generate all [Auto-generated Content](#Auto-generated-content).
* test - depends on setup
  * Runs linting and all testcases

### Devcontainer / gitpod.io

This project can be run as a vscode devcontainer and/or in gitpod.io.

To work with this project in gitpod go to: <https://gitpod.io/#https://github.com/erlang/erlang-org/>

For instructions on how to run with vscode devcontainers see: <https://code.visualstudio.com/docs/remote/containers>

## Adding content

Most pages are either html or markdown pages so they can be edited directly. They
are located in the at the same place as the URL. So, for instance, the `/about` URL
is implemented by [/about.md](/about.md) and `/community/euc` is implemented in
[/community/euc](/community/euc.md).

There are three major [collections](https://jekyllrb.com/docs/collections/) that
you can add new items to: [News], [Blog] and [Release]. Each of these are
markdown files found in _news, _posts and _releases respectively. There is a
README file in each of those folders that describe the mandatory front matter
for each item.

The markdown dialect used is [github flavored markdown](https://github.github.com/gfm/).

There are also two yaml data files that contain the [documentation] and [community] links.

[News]: /_news/README.md
[Blog]: /_posts/README.md
[Release]: /_releases/README.md
[documentation]: [/_data/doc-links.yaml]
[community]: [/_data/community-links.yaml]

## Auto-generated content

When doing `make setup` the auto-generated content is created. All auto-generated
content is cached on github in order to speed up the netlify build. 

### EEPs

This is placed under `_eeps`.

Clone <https://github.com/erlang/eep> then parse using [format-eeps.erl]. We do not use
the perl markdown formatter for EEPs as the html produced does not look very nice.

[format-eeps.erl]: _scripts/src/format-eeps.erl

### FAQ

This is placed under `faq`.

Clone <https://github.com/matthiasl/Erlang-FAQ> and then build it.

### Patches

This is placed under `_data/release.json` and `_patches`.

We fetch the latest [otp_versions.table] and from there use the [Github API](https://docs.github.com/en/rest)
and use erlang.org rsync to fetch information about each patch released since OTP-17.0.

`_patches/tickets.json` maps every `OTP-`, `PR-` and `GH-` id mentioned in a release's notes to the releases
that mention it, which is what lets the [version tree](#versions) be searched by ticket. It is written here,
from the parsed readmes, rather than derived later from the generated pages: the ids are already structured at
that point, and recovering them from our own rendered YAML would be fragile.

The files in `_patches` and `_data/release.json` contain a lot of duplicate information. We could have kept the
`_data/release.json` as the only place to keep the data, but we didn't as doing lookups in it turned out to
be too slow for jekyll.

[otp_versions.table]: https://github.com/erlang/otp/blob/master/otp_versions.table

### Versions

This is placed under `assets/otp-versions.json` and `_versions`, and drives the
[version tree](https://www.erlang.org/versions) page. The generator is [create-versions.erl]; the page is
[otp-versions.ts] and [otp-versions.scss].

#### Where the data comes from

* **Applications** — [otp_versions.table], which says what each version contains and which of those
  applications changed in it.
* **Dates** — the `OTP-*` git tag dates, read through the [Github GraphQL API](https://docs.github.com/en/graphql).
  Not the GitHub release dates: tags go back to 2014 while releases only start at OTP 21, and 86 of the older
  ones were backfilled later with the date of the backfill rather than of the release (every OTP 21.0.x release
  is stamped 2020-09-25). One GraphQL page per 100 tags, against ~570 REST calls.
* **Advisories** — `openvex.table` from the [openvex] branch, joined with the repository security advisories
  for GHSA ids, severities and summaries, which openvex does not carry.
* **Ticket ids** — `tickets.json` out of the [patches](#patches) cache, copied to
  `assets/otp-tickets.json` and fetched separately by the page.

#### Decisions worth knowing

* **The two caches do not depend on each other.** The ticket index is produced by the patches generator and
  reaches the page as its own file, rather than being folded into the version data. Otherwise generating
  `_versions` would mean having `_patches` in place first, which is an ordering to get wrong under `make -j`
  and a staleness to reason about on every build. As separate files each cache refreshes on its own and the
  page joins the latest of both. Only the releases that introduce a ticket are recorded; everything ordered
  above one of them contains it, which the page works out.
* **The generator emits facts; the page derives structure.** Which branch a version is on, how two versions
  are ordered, and whether a release still carries an advisory are all computed in the browser from the
  version numbers. Emitting them would mean maintaining the ordering algorithm in both Erlang and
  TypeScript, and it is the one piece that must not drift.
* **Advisory exposure runs through that same ordering.** A release is affected when its version of the
  application is ordered *below* the version that fixed it. Where the two have no defined order nothing is
  claimed — the page says "undetermined" rather than guessing. Every current branch head comes out clean,
  which is the check that the join is right.
* **Bundled components are shown, not dropped.** Every openvex statement about zlib, OpenSSL, PCRE2,
  wxWidgets and the OpenGL refpages is a *dismissal* — none carries a fix version. They exist to answer the
  scanners that flag the bundled copy, so they are listed under *Bundled components*, apart from the
  advisories and labelled as assessments of a major release rather than of one version.
* **A resolved advisory supersedes a stale assessment.** `openvex.table` still lists CVE-2025-58050 against
  the bundled PCRE2 as `under_investigation` while the same CVE already has a fixed advisory against erts.
  The page suppresses the assessment for that id and major, or it would report one vulnerability as both
  open and unassessed.
* **Support status is `latest - 3`,** per [SECURITY.md]. It also decides which releases are expanded by
  default, so the page opens on what still receives updates and follows the policy on its own.
* **The stop marker means "last release on this line", not "unmaintained".** It is a fact from the table;
  `27.3.4.16` and `26.2.5.21` both carry it and both are recent.
* **One visual device per meaning.** Line weight separates the main track from branches; a node's fill says
  what it is (a release, tinted by its ordering relation, or the red stop at the end of a branch); a ring
  says it is selected. Adding a colour means taking one of those meanings away.
* **Branches are drawn above the version they branched off,** because every release on them is newer than
  that version, and the page lists newest first.

#### Building it

Generating needs Erlang and a few hundred GitHub API calls, neither of which the Netlify build has, so the
result is cached in the `_versions` branch the same way `_patches` is, and refreshed by
[update-gh-cache.yaml]. Two differences from `_patches`, both because of that constraint: a production build
never regenerates (`JEKYLL_ENV=production`, as `docs` does) so a stale cache cannot fail the site build, and
`make versions` builds into a temporary directory and swaps, so a failed run cannot leave the cache empty.

`assets/otp-versions.json` is *copied* out of the cache rather than symlinked into it as
`_data/releases.json` is: jekyll follows a symlinked directory but copies a symlinked file as the link
itself, which dangles in `_site`. The page's class names are partly built at runtime, so `/^otpv-/` and
`/^sev-/` are on the purgecss safelist.

[create-versions.erl]: _scripts/src/create-versions.erl
[otp-versions.ts]: assets/otp-versions.ts
[otp-versions.scss]: assets/css/otp-versions.scss
[openvex]: https://github.com/erlang/otp/tree/openvex
[SECURITY.md]: https://github.com/erlang/otp/blob/master/SECURITY.md
[update-gh-cache.yaml]: .github/workflows/update-gh-cache.yaml

### Documentation

This is placed under `docs`.

The latest documentation for each release since OTP-17 is downloaded+flattened and put into the `docs` folder.
The documentation is not built from scratch but rather fetched from github releases or erlang.org.

The documentation in `docs/doc` is modified to have the algolia search functionality inserted into it.

## Algolia

We have an agreement with algolia that they run a scraper that goes through our
documentation and provides search results from that. We use a customized
[Algolia Crawler] that crawls the documentation at www.erlang.org/doc once every week.

For the search widget we use [docsearch v3], which is a small react widget.

At the moment there is (as far as I know) no good way for a anybody else to
optimize the search results as the crawler and index config is inside my (@garazdawi)
account and not available outside. So if you want to attempt to make the search 
better results, you should contact me and we'll have to work together to improve
things.

Getting good results from the search is hard, so maybe we should
implement a way to make sure that `lists:map` is recognized as a module
and function. However, our react skills are not there yet so this will
have to do for now.

[Algloia Crawler]: https://www.algolia.com/doc/tools/crawler/getting-started/overview/
[docsearch v3]: https://docsearch.algolia.com/docs/DocSearch-v3

## Prerequisites

You need to have the following tools installed to build the erlang.org site:

* GNU make 4.1
* ruby 3.3.0
* bundler 2.5.6
* nodejs 22.14.0
* erlang 26 and 27
* xsltproc
* jq 1.6
* asdf 0.15

Most likely others versions of these tools will work, but they have not been tested.

If you want to be sure that you use the correct version of the dependencies you can either
use the [devcontainer](#devcontainer--gitpodio) or [asdf](https://asdf-vm.com/).

### Using asdf

To install and use asdf follow their [Getting started guide](https://asdf-vm.com/guide/getting-started.html). On Linux using bash in a nutshell you do this:

```shell
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.15.0
echo ". $HOME/.asdf/asdf.sh" >> ~/.bashrc
echo ". $HOME/.asdf/completions/asdf.bash" >> ~/.bashrc
. ~/.bashrc
asdf plugin add erlang
asdf plugin add nodejs
asdf plugin add ruby
```

If you have any issues please refer to the asdf documentation.

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

If you want to change the color of a specific component there is a list of the sass variable that you need to change in the specific page. For example if you want to change the font-size in badges you can lookup the variable here: <https://getbootstrap.com/docs/5.0/components/badge/#sass>. And then set `$badge-font-size: 0.80em` in [_variables.scss](_sass/_variables.scss]).

A full list of all the variables can be found in `node_modules/bootstrap/scss/_variables.scss`.

You can of course also create your own styles, but we try to stay with the bootstrap styles as much as possible.

[bootstrap docs]: https://getbootstrap.com/docs/5.1/

## Architecture

## Things to do when switching

- [ ] Change www.erlang.org to point to new cdn.

### Redirection fixes
- [x] Redirect blog.erlang.org/* to www.erlang.org/blog
- [x] Redirect bugs.erlang.org/browse/* to www.erlang.org/bugs/
- [x] Redirect bugs.erlang.org/* to github.com/erlang/otp/issues
- [ ] Redirect erlang.org/faq/* to www.erlang.org/faq
- [ ] Redirect erlang.org/eep/* to www.erlang.org/eep
- [ ] Redirect erlang.org/eeps/* to www.erlang.org/eeps
- [ ] Redirect erlang.org/doc/* to www.erlang.org/doc
- [ ] Redirect erlang.org/workshop/* to www.erlang.org/workshop
- [x] Redirect www.erlang.org/download/* to erlang.org/download
- [x] Redirect www.erlang.org/~* to erlang.org/~*
- [x] Redirect www.erlang.org/course/* to erlang.org/course
- [x] Redirect www.erlang.org/documentation/* to erlang.org/documentation
- [x] Redirect www.erlang.org/mailman/* to erlang.org/mailman
- [x] Redirect www.erlang.org/mailman-icons/* to erlang.org/mailman-icons
- [x] Redirect www.erlang.org/pipermail/* to erlang.org/pipermail

## Things that have been removed

* The course (this is actually on erlang.org)
* The events (redirect to EEF?)
* Documentation version index page (redirect to erlang.org/documentation)

## Ideas

* Add plausable.io tracking
* Add <https://search.google.com/search-console/> support?
* Add visual testing
  * Use [BackstopJS](https://css-tricks.com/automating-css-regression-testing/) for regression testing 
  * <https://applitools.com/>
* Rework /community to not just be a bunch of links. Maybe the rust page can give some inspiration? <https://www.rust-lang.org/community>
* Add markdownlint? <https://www.npmjs.com/package/markdownlint>
* Add paginated docsearch results. See <https://discourse.algolia.com/t/dedicated-search-page/583> and <https://jsfiddle.net/maxiloc/oemnhuv4/>
* `/docs`
  * Other sections? Learning/Developing/References
* `/community`
  * Beam Languages
  * Other projects
