/*
 * Algolia-backed typeahead for OTP docs.
 *
 * Replaces ExDoc's Lunr autocomplete with an Algolia DocSearch query,
 * scoped to the version being viewed plus (on per-app pages) the
 * current application. Designed to be injected by
 * _scripts/otp_add_headers.sh into every ExDoc-generated page.
 *
 * No bundler, no SDK — keeps the script small and version-agnostic so
 * it works against every OTP/ExDoc version we ship docs for. Will be
 * deleted once https://github.com/elixir-lang/ex_doc gains a JS
 * registerSearchProvider hook and we can use that instead.
 */
(function () {
  'use strict';

  // Public DocSearch credentials — search-only, safe in client code.
  // Mirrors the values in assets/doc-search.tsx and _layouts/search.html;
  // rotate all three together if these ever change.
  var APP_ID = 'LUYTU1J2MB';
  var API_KEY = '86152ba1d4a9d7e179d537b8060a4c31';
  var INDEX = 'erlang';
  var DEBOUNCE_MS = 120;
  var MAX_HITS = 8;

  // Logical groups of OTP applications that get searched together
  // when scope is "app". When the current page is in any group
  // member, the search spans every app in that group. Apps not in
  // any group are scoped to themselves alone.
  var APP_GROUPS = [
    { name: 'core',     apps: ['stdlib', 'kernel', 'erts'] },
    { name: 'security', apps: ['ssl', 'crypto', 'public_key', 'ssh'] },
    { name: 'code',     apps: ['compiler', 'syntax_tools', 'parsetools'] },
    { name: 'test',     apps: ['common_test', 'eunit'] },
    { name: 'system',   apps: ['Erlang System Documentation'] },
  ];
  function groupByName(name) {
    for (var i = 0; i < APP_GROUPS.length; i++) {
      if (APP_GROUPS[i].name === name) return APP_GROUPS[i];
    }
    return null;
  }
  function groupFor(app) {
    if (!app) return null;
    for (var i = 0; i < APP_GROUPS.length; i++) {
      if (APP_GROUPS[i].apps.indexOf(app) >= 0) return APP_GROUPS[i];
    }
    return null;
  }
  function defaultScope(ctx) {
    // Default scope is the group containing the current app when
    // there is one (typical case for apps that have siblings —
    // stdlib defaults to "core"), otherwise the current app alone,
    // otherwise just the version.
    if (!ctx.application) return 'all';
    var g = groupFor(ctx.application);
    return g ? g.name : ctx.application;
  }

  // Tab cycles scope through these ids in order. Suppress the
  // current-app entry when it duplicates a single-member group.
  // On unscoped pages (umbrella index, redirect stubs) there's no
  // natural "current" app, so cycle through every group instead.
  function tabCycle(ctx) {
    if (!ctx.application) {
      var all = APP_GROUPS.map(function (g) { return g.name; });
      all.push('all');
      return all;
    }
    var g = groupFor(ctx.application);
    var cycle = [];
    if (!g || g.apps.length > 1) cycle.push(ctx.application);
    if (g) cycle.push(g.name);
    cycle.push('all');
    return cycle;
  }

  function meta(name) {
    var el = document.querySelector('meta[name="' + name + '"]');
    return el ? el.getAttribute('content') : null;
  }

  // Strip the trailing " v1.2.3" / " v1.2.3-rc0" from a project meta
  // value, leaving just the application name as Algolia indexes it.
  function appFromProject(project) {
    if (!project) return null;
    return project.replace(/\s+v[\d.]+(-[A-Za-z0-9.]+)?\s*$/, '');
  }

  function getContext() {
    var majorVsn = meta('major-vsn');
    var project = meta('project');
    if (!majorVsn) return null;
    // Per-app pages: /doc(s/<v>)?/apps/<app>/...
    // System docs pages: /doc(s/<v>)?/system/... (indexed as the
    //   "Erlang System Documentation" app)
    // Anything else (redirect stubs at /doc/foo.html) gets no app
    // scope — pill is hidden and search is global within the version.
    var path = window.location.pathname;
    var isScopedPage = /\/(apps\/[^/]+|system)\//.test(path);
    return {
      version: majorVsn,
      application: isScopedPage ? appFromProject(project) : null,
    };
  }

  // scope state is one of:
  //   'all'           — no app filter, just the version
  //   '<group name>'  — every app in the named APP_GROUPS entry
  //   '<app name>'    — a single application (current page's app)
  //
  // Persistence stores the *level* — 'app', 'group', or 'all' — not
  // the concrete scope. So a user who searched at the "group" level
  // while in stdlib (group: core) and then navigates to a compiler
  // page gets the "code" group, not still "core". Adapts the
  // resolved scope to the page while preserving the user's intent.
  //
  // On unscoped pages (umbrella index, redirect stubs) there's no
  // page-derived group, so we also remember the last group name the
  // user picked and use it as the tiebreaker when level=group.
  var SCOPE_KEY = 'erlang-org:algolia-scope-level';
  var GROUP_KEY = 'erlang-org:algolia-last-group';
  function scopeToLevel(scope) {
    if (scope === 'all') return 'all';
    if (groupByName(scope)) return 'group';
    return 'app';
  }
  function lastGroup() {
    try {
      var v = sessionStorage.getItem(GROUP_KEY);
      return v && groupByName(v) ? v : null;
    } catch (_) { return null; }
  }
  function levelToScope(level, ctx) {
    if (level === 'all') return 'all';
    if (level === 'group') {
      var g = groupFor(ctx.application);
      if (g) return g.name;
      return lastGroup() || APP_GROUPS[0].name;
    }
    // 'app'
    return ctx.application || 'all';
  }
  function loadScope(ctx) {
    try {
      var level = sessionStorage.getItem(SCOPE_KEY);
      if (level === 'app' || level === 'group' || level === 'all') {
        return levelToScope(level, ctx);
      }
    } catch (_) { /* fall through */ }
    return defaultScope(ctx);
  }
  function saveScope(scope) {
    try {
      sessionStorage.setItem(SCOPE_KEY, scopeToLevel(scope));
      if (groupByName(scope)) sessionStorage.setItem(GROUP_KEY, scope);
    } catch (_) { /* no-op */ }
  }

  function buildFilters(ctx, scope) {
    var parts = ['version:' + ctx.version];
    if (scope === 'all') return parts.join(' AND ');
    var g = groupByName(scope);
    if (g) {
      var or = g.apps.map(function (a) {
        return 'app:' + JSON.stringify(a);
      }).join(' OR ');
      parts.push('(' + or + ')');
    } else {
      // Single app
      parts.push('app:' + JSON.stringify(scope));
    }
    return parts.join(' AND ');
  }

  function search(query, ctx, scope, signal) {
    var analytics = window.__algoliaAnalytics !== false;
    return fetch(
      'https://' + APP_ID + '-dsn.algolia.net/1/indexes/' + INDEX + '/query',
      {
        method: 'POST',
        headers: {
          'X-Algolia-Application-Id': APP_ID,
          'X-Algolia-API-Key': API_KEY,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          query: query,
          filters: buildFilters(ctx, scope),
          hitsPerPage: MAX_HITS,
          analytics: analytics,
          attributesToRetrieve: [
            'hierarchy.lvl1', 'hierarchy.lvl2', 'hierarchy.lvl3',
            'url', 'isModule',
          ],
          attributesToHighlight: [
            'hierarchy.lvl1', 'hierarchy.lvl2', 'hierarchy.lvl3',
          ],
          attributesToSnippet: ['content:30'],
        }),
        signal: signal,
      }
    ).then(function (res) {
      if (!res.ok) throw new Error('algolia ' + res.status);
      return res.json();
    }).then(function (j) {
      return j.hits || [];
    });
  }

  function escapeHtml(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }

  // Algolia returns highlight/snippet values with safe markup around
  // matches (a <span class="algolia-docsearch-suggestion--highlight">),
  // but un-tagged values need HTML escaping.
  function hierarchyHL(hit, lvl) {
    var hl = hit._highlightResult;
    var h = hl && hl.hierarchy && hl.hierarchy[lvl];
    if (h && h.value) return h.value;
    var raw = hit.hierarchy && hit.hierarchy[lvl];
    return raw ? escapeHtml(raw) : '';
  }

  function contentSnippet(hit) {
    var sr = hit._snippetResult;
    return sr && sr.content && sr.content.value ? sr.content.value : '';
  }

  // Title mirrors /doc/search.html's hit-name template: module hits
  // read as `lvl1[:lvl2]` (e.g. "lists:foreach(Fun, List)"), non-module
  // hits as `lvl1[ → lvl2][ → lvl3]` (breadcrumb for guides/system
  // docs/command pages).
  function hitTitle(hit) {
    var lvl1 = hierarchyHL(hit, 'lvl1');
    var lvl2 = hierarchyHL(hit, 'lvl2');
    var lvl3 = hierarchyHL(hit, 'lvl3');
    if (hit.isModule) {
      return lvl2 ? (lvl1 + ':' + lvl2) : (lvl1 || '(untitled)');
    }
    var parts = [lvl1 || '(untitled)'];
    if (lvl2) parts.push(lvl2);
    if (lvl3) parts.push(lvl3);
    return parts.join(' &rsaquo; ');
  }

  function renderHits(target, hits, query, scope) {
    if (!hits.length) {
      var hint = '';
      if (scope !== 'all') {
        hint = ' Try widening the scope (Tab).';
      }
      target.innerHTML =
        '<div class="alg-empty">No results for ' +
        '<strong>' + escapeHtml(query) + '</strong>.' + hint + '</div>';
      return;
    }
    var html = '';
    for (var i = 0; i < hits.length; i++) {
      var h = hits[i];
      var title = hitTitle(h);
      var excerpt = contentSnippet(h);
      html +=
        '<a class="alg-hit" href="' + escapeHtml(h.url) +
        '" data-index="' + i + '" role="option">' +
        '<div class="alg-hit-title">' + title + '</div>' +
        (excerpt ? '<div class="alg-hit-excerpt">' + excerpt + '</div>' : '') +
        '</a>';
    }
    target.innerHTML = html;
  }

  function debounce(fn, ms) {
    var t;
    return function () {
      var self = this, args = arguments;
      clearTimeout(t);
      t = setTimeout(function () { fn.apply(self, args); }, ms);
    };
  }

  function mount(input, ctx) {
    // Clone the input to detach ExDoc's Lunr listeners. The `/`
    // keyboard shortcut is bound on document and finds the input by
    // class, so it still focuses the replacement.
    var fresh = input.cloneNode(true);
    input.parentNode.replaceChild(fresh, input);
    input = fresh;

    var wrap = document.createElement('div');
    wrap.className = 'alg-dropdown';
    wrap.setAttribute('role', 'listbox');
    wrap.hidden = true;

    // Scope pill row — one pill per APP_GROUPS entry, plus the
    // current page's app when it's not in any group, plus an
    // "all OTP" pill that disables the app filter.
    var scope = loadScope(ctx);
    var pillButtons = []; // [{ id, el }]

    var pillRow = document.createElement('div');
    pillRow.className = 'alg-scope';
    pillRow.appendChild(document.createTextNode('Searching: '));

    function makePill(id, label, title) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'alg-scope-pill';
      b.setAttribute('data-scope', id);
      b.textContent = label;
      if (title) b.title = title;
      b.addEventListener('click', function () { setScope(id); input.focus(); });
      pillButtons.push({ id: id, el: b });
      return b;
    }

    // Current app pill goes first (default scope), unless it would
    // duplicate a single-member group (e.g. system).
    if (ctx.application) {
      var ownGroup = groupFor(ctx.application);
      if (!ownGroup || ownGroup.apps.length > 1) {
        pillRow.appendChild(makePill(ctx.application, ctx.application, ctx.application));
      }
    }
    for (var gi = 0; gi < APP_GROUPS.length; gi++) {
      var g = APP_GROUPS[gi];
      pillRow.appendChild(makePill(g.name, g.name, g.apps.join(', ')));
    }
    pillRow.appendChild(makePill('all', 'all OTP', 'every OTP application in this version'));

    wrap.appendChild(pillRow);

    var results = document.createElement('div');
    results.className = 'alg-results';

    var footer = document.createElement('div');
    footer.className = 'alg-footer';
    if (tabCycle(ctx).length > 1) {
      var hint = document.createElement('span');
      hint.className = 'alg-footer-hint';
      hint.innerHTML = '<kbd>Tab</kbd> to switch scope';
      footer.appendChild(hint);
    }
    var attribution = document.createElement('a');
    attribution.className = 'alg-footer-attribution';
    attribution.href = 'https://www.algolia.com/';
    attribution.target = '_blank';
    attribution.rel = 'noopener';
    attribution.innerHTML = 'Search by <strong>Algolia</strong>';
    footer.appendChild(attribution);

    wrap.appendChild(results);
    wrap.appendChild(footer);

    var host = input.closest('.search-bar') || input.parentNode;
    host.style.position = host.style.position || 'relative';
    host.appendChild(wrap);

    var ctrl = null;
    var hits = [];
    var selected = -1;

    function updateScopePills() {
      for (var i = 0; i < pillButtons.length; i++) {
        pillButtons[i].el.classList.toggle('alg-active', pillButtons[i].id === scope);
      }
    }
    updateScopePills();

    function setScope(next) {
      if (next === scope) return;
      scope = next;
      saveScope(scope);
      updateScopePills();
      if (input.value.trim()) runQuery();
    }

    function tabToNextScope() {
      var cycle = tabCycle(ctx);
      if (cycle.length <= 1) return;
      var i = cycle.indexOf(scope);
      var next = i < 0 ? cycle[0] : cycle[(i + 1) % cycle.length];
      setScope(next);
    }


    function updateSelected() {
      var els = results.querySelectorAll('.alg-hit');
      for (var i = 0; i < els.length; i++) {
        if (i === selected) {
          els[i].classList.add('alg-selected');
          els[i].setAttribute('aria-selected', 'true');
          els[i].scrollIntoView({ block: 'nearest' });
        } else {
          els[i].classList.remove('alg-selected');
          els[i].removeAttribute('aria-selected');
        }
      }
    }

    var loadingTimer = null;
    function runQuery() {
      var q = input.value.trim();
      if (ctrl) ctrl.abort();
      if (loadingTimer) clearTimeout(loadingTimer);
      if (!q) {
        wrap.hidden = true;
        results.innerHTML = '';
        hits = [];
        selected = -1;
        return;
      }
      // Show a "Searching…" placeholder if the request takes longer
      // than ~200 ms, so slow networks have visible feedback without
      // flickering on fast ones.
      loadingTimer = setTimeout(function () {
        results.innerHTML = '<div class="alg-empty alg-loading">Searching…</div>';
        wrap.hidden = false;
      }, 200);
      ctrl = new AbortController();
      search(q, ctx, scope, ctrl.signal).then(function (rs) {
        clearTimeout(loadingTimer);
        hits = rs;
        selected = -1;
        renderHits(results, hits, q, scope);
        wrap.hidden = false;
      }).catch(function (err) {
        clearTimeout(loadingTimer);
        if (err.name === 'AbortError') return;
        // Network / Algolia error — fall back to a link to the full
        // search page so the user still has a way to query.
        var fallback = '/doc/search.html?v=' + encodeURIComponent(ctx.version) +
                       '&q=' + encodeURIComponent(q);
        results.innerHTML =
          '<div class="alg-empty">Search unavailable. ' +
          '<a href="' + fallback + '">Open full search</a>.</div>';
        wrap.hidden = false;
        // Log in case it's not just transient.
        if (window.console && console.warn) console.warn('algolia-typeahead:', err);
      });
    }

    var onInput = debounce(runQuery, DEBOUNCE_MS);
    input.addEventListener('input', onInput);

    input.addEventListener('focus', function () {
      if (hits.length) wrap.hidden = false;
    });

    // Capture-phase handler for Enter and Tab so they run *before*
    // ExDoc's listeners (Enter triggers their form submit; Tab
    // would move focus out of the input). stopImmediatePropagation
    // keeps ExDoc from acting on them.
    input.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' && !wrap.hidden && selected >= 0 && hits[selected]) {
        e.preventDefault();
        e.stopImmediatePropagation();
        window.location.href = hits[selected].url;
        return;
      }
      if (e.key === 'Tab' && !e.shiftKey && !e.altKey && !e.ctrlKey && !e.metaKey
          && tabCycle(ctx).length > 1) {
        e.preventDefault();
        e.stopImmediatePropagation();
        tabToNextScope();
        return;
      }
    }, true);

    // Bubble-phase handler for navigation keys. Mirrors the bindings
    // ExDoc's own search-bar.js uses so users with muscle memory get
    // the same behaviour:
    //   Up / Ctrl+P    -> select previous hit
    //   Down / Ctrl+N  -> select next hit
    //   Escape         -> close dropdown and blur the input
    input.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') {
        if (!wrap.hidden) {
          wrap.hidden = true;
          selected = -1;
        }
        input.blur();
        return;
      }

      if (wrap.hidden) return;
      var nextHit = e.key === 'ArrowDown' || (e.ctrlKey && e.key === 'n');
      var prevHit = e.key === 'ArrowUp'   || (e.ctrlKey && e.key === 'p');
      if (nextHit) {
        e.preventDefault();
        selected = Math.min(selected + 1, hits.length - 1);
        updateSelected();
      } else if (prevHit) {
        e.preventDefault();
        selected = Math.max(selected - 1, -1);
        updateSelected();
      }
    });

    document.addEventListener('click', function (e) {
      if (e.target === input) return;
      if (wrap.contains(e.target)) return;
      wrap.hidden = true;
    });
  }

  function init() {
    var ctx = getContext();
    if (!ctx) {
      // Missing meta tags — page probably isn't an ExDoc page we care about.
      return;
    }
    var input = document.querySelector('.search-input');
    if (!input) return;
    mount(input, ctx);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
