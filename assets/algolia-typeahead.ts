/*
 * Algolia-backed typeahead for OTP docs.
 *
 * Replaces ExDoc's Lunr autocomplete with an Algolia DocSearch query,
 * scoped to the version being viewed plus (on per-app pages) the
 * current application. Designed to be injected by
 * _scripts/otp_add_headers.sh into every ExDoc-generated page.
 *
 * No SDK — keeps the script small and version-agnostic so it works
 * against every OTP/ExDoc version we ship docs for. Will be deleted
 * once https://github.com/elixir-lang/ex_doc gains a JS
 * registerSearchProvider hook and we can use that instead.
 */

// Public DocSearch credentials — search-only, safe in client code.
// Mirrors the values in assets/doc-search.tsx and _layouts/search.html;
// rotate all three together if these ever change.
const APP_ID = 'LUYTU1J2MB';
const API_KEY = '86152ba1d4a9d7e179d537b8060a4c31';
const INDEX = 'erlang';
const DEBOUNCE_MS = 120;
const MAX_HITS = 8;

interface AppGroup {
  name: string;
  apps: string[];
}

// Logical groups of OTP applications that get searched together
// when scope is "app". When the current page is in any group
// member, the search spans every app in that group. Apps not in
// any group are scoped to themselves alone.
const APP_GROUPS: ReadonlyArray<AppGroup> = [
  { name: 'core',     apps: ['stdlib', 'kernel', 'erts'] },
  { name: 'security', apps: ['ssl', 'crypto', 'public_key', 'ssh'] },
  { name: 'code',     apps: ['compiler', 'syntax_tools', 'parsetools'] },
  { name: 'test',     apps: ['common_test', 'eunit'] },
  { name: 'system',   apps: ['Erlang System Documentation'] },
];

interface Context {
  version: string;
  application: string | null;
}

interface Hit {
  url: string;
  isModule?: boolean;
  hierarchy?: Partial<Record<HierarchyLevel, string>>;
  _highlightResult?: {
    hierarchy?: Partial<Record<HierarchyLevel, { value: string }>>;
  };
  _snippetResult?: {
    content?: { value: string };
  };
}

type HierarchyLevel = 'lvl1' | 'lvl2' | 'lvl3';
type ScopeLevel = 'app' | 'group' | 'all';

function groupByName(name: string): AppGroup | null {
  return APP_GROUPS.find(g => g.name === name) ?? null;
}

function groupFor(app: string | null): AppGroup | null {
  if (!app) return null;
  return APP_GROUPS.find(g => g.apps.indexOf(app) >= 0) ?? null;
}

function defaultScope(ctx: Context): string {
  // Default scope is the group containing the current app when
  // there is one (typical case for apps that have siblings —
  // stdlib defaults to "core"), otherwise the current app alone,
  // otherwise just the version.
  if (!ctx.application) return 'all';
  const g = groupFor(ctx.application);
  return g ? g.name : ctx.application;
}

// Tab cycles scope through these ids in order. Suppress the
// current-app entry when it duplicates a single-member group.
// On unscoped pages (umbrella index, redirect stubs) there's no
// natural "current" app, so cycle through every group instead.
function tabCycle(ctx: Context): string[] {
  if (!ctx.application) {
    return [...APP_GROUPS.map(g => g.name), 'all'];
  }
  const g = groupFor(ctx.application);
  const cycle: string[] = [];
  if (!g || g.apps.length > 1) cycle.push(ctx.application);
  if (g) cycle.push(g.name);
  cycle.push('all');
  return cycle;
}

function meta(name: string): string | null {
  const el = document.querySelector<HTMLMetaElement>('meta[name="' + name + '"]');
  return el ? el.getAttribute('content') : null;
}

// Strip the trailing " v1.2.3" / " v1.2.3-rc0" from a project meta
// value, leaving just the application name as Algolia indexes it.
function appFromProject(project: string | null): string | null {
  if (!project) return null;
  return project.replace(/\s+v[\d.]+(-[A-Za-z0-9.]+)?\s*$/, '');
}

function getContext(): Context | null {
  const majorVsn = meta('major-vsn');
  const project = meta('project');
  if (!majorVsn) return null;
  // Per-app pages: /doc(s/<v>)?/apps/<app>/...
  // System docs pages: /doc(s/<v>)?/system/... (indexed as the
  //   "Erlang System Documentation" app)
  // Anything else (redirect stubs at /doc/foo.html) gets no app
  // scope — pill is hidden and search is global within the version.
  const isScopedPage = /\/(apps\/[^/]+|system)\//.test(window.location.pathname);
  return {
    version: majorVsn,
    application: isScopedPage ? appFromProject(project) : null,
  };
}

// Persistence stores the scope *level* — 'app', 'group', or 'all' —
// not the concrete scope. So a user who searched at the "group" level
// while in stdlib (group: core) and then navigates to a compiler page
// gets the "code" group, not still "core". On unscoped pages (umbrella
// index, redirect stubs) there's no page-derived group, so we also
// remember the last group name the user picked and use it as the
// tiebreaker when level=group.
const SCOPE_KEY = 'erlang-org:algolia-scope-level';
const GROUP_KEY = 'erlang-org:algolia-last-group';

function safeGet(key: string): string | null {
  try { return sessionStorage.getItem(key); } catch { return null; }
}
function safeSet(key: string, value: string): void {
  try { sessionStorage.setItem(key, value); } catch { /* no-op */ }
}

function scopeToLevel(scope: string): ScopeLevel {
  if (scope === 'all') return 'all';
  if (groupByName(scope)) return 'group';
  return 'app';
}

function lastGroup(): string | null {
  const v = safeGet(GROUP_KEY);
  return v && groupByName(v) ? v : null;
}

function levelToScope(level: ScopeLevel, ctx: Context): string {
  if (level === 'all') return 'all';
  if (level === 'group') {
    const g = groupFor(ctx.application);
    if (g) return g.name;
    return lastGroup() ?? APP_GROUPS[0].name;
  }
  return ctx.application ?? 'all';
}

function loadScope(ctx: Context): string {
  const level = safeGet(SCOPE_KEY);
  if (level === 'app' || level === 'group' || level === 'all') {
    return levelToScope(level, ctx);
  }
  return defaultScope(ctx);
}

function saveScope(scope: string): void {
  safeSet(SCOPE_KEY, scopeToLevel(scope));
  if (groupByName(scope)) safeSet(GROUP_KEY, scope);
}

function buildFilters(ctx: Context, scope: string): string {
  const parts = ['version:' + ctx.version];
  if (scope === 'all') return parts.join(' AND ');
  const g = groupByName(scope);
  if (g) {
    const or = g.apps.map(a => 'app:' + JSON.stringify(a)).join(' OR ');
    parts.push('(' + or + ')');
  } else {
    parts.push('app:' + JSON.stringify(scope));
  }
  return parts.join(' AND ');
}

async function search(query: string, ctx: Context, scope: string, signal: AbortSignal): Promise<Hit[]> {
  const analytics = (window as { __algoliaAnalytics?: boolean }).__algoliaAnalytics !== false;
  const res = await fetch(
    'https://' + APP_ID + '-dsn.algolia.net/1/indexes/' + INDEX + '/query',
    {
      method: 'POST',
      headers: {
        'X-Algolia-Application-Id': APP_ID,
        'X-Algolia-API-Key': API_KEY,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        query,
        filters: buildFilters(ctx, scope),
        hitsPerPage: MAX_HITS,
        analytics,
        attributesToRetrieve: [
          'hierarchy.lvl1', 'hierarchy.lvl2', 'hierarchy.lvl3',
          'url', 'isModule',
        ],
        attributesToHighlight: [
          'hierarchy.lvl1', 'hierarchy.lvl2', 'hierarchy.lvl3',
        ],
        attributesToSnippet: ['content:30'],
      }),
      signal,
    }
  );
  if (!res.ok) throw new Error('algolia ' + res.status);
  const json = await res.json() as { hits?: Hit[] };
  return json.hits ?? [];
}

const HTML_ESCAPE: Record<string, string> = {
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
};
function escapeHtml(s: string): string {
  return String(s).replace(/[&<>"']/g, c => HTML_ESCAPE[c]);
}

// Algolia returns highlight/snippet values with safe markup around
// matches (a <span class="algolia-docsearch-suggestion--highlight">),
// but un-tagged values need HTML escaping.
function hierarchyHL(hit: Hit, lvl: HierarchyLevel): string {
  const h = hit._highlightResult?.hierarchy?.[lvl];
  if (h?.value) return h.value;
  const raw = hit.hierarchy?.[lvl];
  return raw ? escapeHtml(raw) : '';
}

function contentSnippet(hit: Hit): string {
  return hit._snippetResult?.content?.value ?? '';
}

// Title mirrors /doc/search.html's hit-name template: module hits
// read as `lvl1[:lvl2]` (e.g. "lists:foreach(Fun, List)"), non-module
// hits as `lvl1[ → lvl2][ → lvl3]` (breadcrumb for guides/system
// docs/command pages).
function hitTitle(hit: Hit): string {
  const lvl1 = hierarchyHL(hit, 'lvl1');
  const lvl2 = hierarchyHL(hit, 'lvl2');
  const lvl3 = hierarchyHL(hit, 'lvl3');
  if (hit.isModule) {
    return lvl2 ? (lvl1 + ':' + lvl2) : (lvl1 || '(untitled)');
  }
  const parts = [lvl1 || '(untitled)'];
  if (lvl2) parts.push(lvl2);
  if (lvl3) parts.push(lvl3);
  return parts.join(' &rsaquo; ');
}

function renderHits(target: HTMLElement, hits: Hit[], query: string, scope: string): void {
  if (!hits.length) {
    const hint = scope !== 'all' ? ' Try widening the scope (Tab).' : '';
    target.innerHTML =
      '<div class="alg-empty">No results for ' +
      '<strong>' + escapeHtml(query) + '</strong>.' + hint + '</div>';
    return;
  }
  let html = '';
  for (let i = 0; i < hits.length; i++) {
    const h = hits[i];
    const title = hitTitle(h);
    const excerpt = contentSnippet(h);
    html +=
      '<a class="alg-hit" href="' + escapeHtml(h.url) +
      '" data-index="' + i + '" role="option">' +
      '<div class="alg-hit-title">' + title + '</div>' +
      (excerpt ? '<div class="alg-hit-excerpt">' + excerpt + '</div>' : '') +
      '</a>';
  }
  target.innerHTML = html;
}

function debounce<T extends unknown[]>(fn: (...args: T) => void, ms: number): (...args: T) => void {
  let t: ReturnType<typeof setTimeout> | undefined;
  return function (this: unknown, ...args: T) {
    if (t !== undefined) clearTimeout(t);
    t = setTimeout(() => fn.apply(this, args), ms);
  };
}

function mount(input: HTMLInputElement, ctx: Context): void {
  // Clone the input to detach ExDoc's Lunr listeners. The `/`
  // keyboard shortcut is bound on document and finds the input by
  // class, so it still focuses the replacement.
  const fresh = input.cloneNode(true) as HTMLInputElement;
  input.parentNode!.replaceChild(fresh, input);
  input = fresh;

  const wrap = document.createElement('div');
  wrap.className = 'alg-dropdown';
  wrap.setAttribute('role', 'listbox');
  wrap.hidden = true;

  let scope = loadScope(ctx);
  const cycle = tabCycle(ctx);

  const pillRow = document.createElement('div');
  pillRow.className = 'alg-scope';
  pillRow.appendChild(document.createTextNode('Searching: '));

  function makePill(id: string, label: string, title?: string): HTMLButtonElement {
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'alg-scope-pill';
    b.dataset.scope = id;
    b.textContent = label;
    if (title) b.title = title;
    b.addEventListener('click', () => { setScope(id); input.focus(); });
    return b;
  }

  // Current app pill goes first (default scope), unless it would
  // duplicate a single-member group (e.g. system).
  if (ctx.application) {
    const ownGroup = groupFor(ctx.application);
    if (!ownGroup || ownGroup.apps.length > 1) {
      pillRow.appendChild(makePill(ctx.application, ctx.application, ctx.application));
    }
  }
  for (const g of APP_GROUPS) {
    pillRow.appendChild(makePill(g.name, g.name, g.apps.join(', ')));
  }
  pillRow.appendChild(makePill('all', 'all OTP', 'every OTP application in this version'));

  wrap.appendChild(pillRow);

  const results = document.createElement('div');
  results.className = 'alg-results';

  const footer = document.createElement('div');
  footer.className = 'alg-footer';
  if (cycle.length > 1) {
    const hint = document.createElement('span');
    hint.className = 'alg-footer-hint';
    hint.innerHTML = '<kbd>Tab</kbd> to switch scope';
    footer.appendChild(hint);
  }
  const attribution = document.createElement('a');
  attribution.className = 'alg-footer-attribution';
  attribution.href = 'https://www.algolia.com/';
  attribution.target = '_blank';
  attribution.rel = 'noopener';
  attribution.innerHTML = 'Search by <strong>Algolia</strong>';
  footer.appendChild(attribution);

  wrap.appendChild(results);
  wrap.appendChild(footer);

  const host = (input.closest('.search-bar') as HTMLElement | null) ?? (input.parentNode as HTMLElement);
  host.style.position = host.style.position || 'relative';
  host.appendChild(wrap);

  let ctrl: AbortController | null = null;
  let hits: Hit[] = [];
  let selected = -1;

  function updateScopePills(): void {
    pillRow.querySelectorAll<HTMLButtonElement>('[data-scope]').forEach(b => {
      b.classList.toggle('alg-active', b.dataset.scope === scope);
    });
  }
  updateScopePills();

  function setScope(next: string): void {
    if (next === scope) return;
    scope = next;
    saveScope(scope);
    updateScopePills();
    if (input.value.trim()) runQuery();
  }

  function tabToNextScope(): void {
    if (cycle.length <= 1) return;
    const i = cycle.indexOf(scope);
    const next = i < 0 ? cycle[0] : cycle[(i + 1) % cycle.length];
    setScope(next);
  }

  function updateSelected(): void {
    const els = results.querySelectorAll<HTMLElement>('.alg-hit');
    for (let i = 0; i < els.length; i++) {
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

  let loadingTimer: ReturnType<typeof setTimeout> | null = null;
  function runQuery(): void {
    const q = input.value.trim();
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
    loadingTimer = setTimeout(() => {
      results.innerHTML = '<div class="alg-empty alg-loading">Searching…</div>';
      wrap.hidden = false;
    }, 200);
    ctrl = new AbortController();
    search(q, ctx, scope, ctrl.signal).then(rs => {
      if (loadingTimer) clearTimeout(loadingTimer);
      hits = rs;
      selected = -1;
      renderHits(results, hits, q, scope);
      wrap.hidden = false;
    }).catch((err: unknown) => {
      if (loadingTimer) clearTimeout(loadingTimer);
      if (err instanceof DOMException && err.name === 'AbortError') return;
      // Network / Algolia error — fall back to a link to the full
      // search page so the user still has a way to query.
      const fallback = '/doc/search.html?v=' + encodeURIComponent(ctx.version) +
                       '&q=' + encodeURIComponent(q);
      results.innerHTML =
        '<div class="alg-empty">Search unavailable. ' +
        '<a href="' + fallback + '">Open full search</a>.</div>';
      wrap.hidden = false;
      console.warn('algolia-typeahead:', err);
    });
  }

  const onInput = debounce(runQuery, DEBOUNCE_MS);
  input.addEventListener('input', onInput);

  input.addEventListener('focus', () => {
    if (hits.length) wrap.hidden = false;
  });

  // Capture-phase handler for Enter and Tab so they run *before*
  // ExDoc's listeners (Enter triggers their form submit; Tab
  // would move focus out of the input). stopImmediatePropagation
  // keeps ExDoc from acting on them.
  input.addEventListener('keydown', e => {
    if (e.key === 'Enter' && !wrap.hidden && selected >= 0 && hits[selected]) {
      e.preventDefault();
      e.stopImmediatePropagation();
      window.location.href = hits[selected].url;
      return;
    }
    if (e.key === 'Tab' && !e.shiftKey && !e.altKey && !e.ctrlKey && !e.metaKey
        && cycle.length > 1) {
      e.preventDefault();
      e.stopImmediatePropagation();
      tabToNextScope();
      return;
    }
  }, true);

  // Bubble-phase navigation. Mirrors ExDoc's search-bar.js bindings
  // (Up/Ctrl-P, Down/Ctrl-N, Escape) so users with muscle memory get
  // the same behaviour.
  input.addEventListener('keydown', e => {
    if (e.key === 'Escape') {
      if (!wrap.hidden) {
        wrap.hidden = true;
        selected = -1;
      }
      input.blur();
      return;
    }

    if (wrap.hidden) return;
    const nextHit = e.key === 'ArrowDown' || (e.ctrlKey && e.key === 'n');
    const prevHit = e.key === 'ArrowUp'   || (e.ctrlKey && e.key === 'p');
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

  document.addEventListener('click', e => {
    if (e.target === input) return;
    if (e.target instanceof Node && wrap.contains(e.target)) return;
    wrap.hidden = true;
  });
}

function init(): void {
  const ctx = getContext();
  if (!ctx) {
    // Missing meta tags — page probably isn't an ExDoc page we care about.
    return;
  }
  const input = document.querySelector<HTMLInputElement>('.search-input');
  if (!input) return;
  mount(input, ctx);
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

export {};
