/**
 * The Erlang/OTP version tree.
 *
 * `create-versions` emits the raw facts: which application versions each
 * Erlang/OTP version contains, when it was tagged, and which CVEs were fixed
 * in which application version. Everything structural is derived here, because
 * a version number on its own determines it:
 *
 *   - which branch a version sits on, and what that branch is based on;
 *   - how two versions are ordered, or that they are not ordered at all;
 *   - and, from that ordering, whether a release still carries an advisory.
 *
 * Those rules are in ./otp-version-scheme.ts, apart from the page so that they
 * can be tested without one.
 */

import { compare, branchOf, baseOf, inAnyRange } from "./otp-version-scheme";

interface Version {
  v: string;
  d: string | null;
  c: number[];
  s: number[];
}

/** What openvex knows: which application version carries and fixes an advisory. */
interface Advisory {
  major: string;
  cve: string;
  app: string;
  introduced: string;
  fixed: string;
}

/**
 * An assessment that a vulnerability in a bundled third-party component does
 * not affect Erlang/OTP. These carry no fix version — there is nothing to fix
 * — and apply to a major release as a whole rather than to one version. They
 * are here to answer the scanners that flag the bundled copy.
 */
interface NotAffected {
  major: string;
  id: string;
  component: string;
  ref: string | null;
  apps: string[];
  justification: string;
}

/**
 * The CVE record for an advisory, where one exists. `releases` says which
 * releases are affected in release terms, which openvex cannot: it is organised
 * per supported major, so it says nothing about anything older. Only the CVEs
 * assigned by the Erlang Ecosystem Foundation carry it, so it is optional and
 * openvex remains the fallback.
 */
interface CveRecord {
  ghsa: string | null;
  severity: Severity | null;
  summary: string | null;
  url: string | null;
  releases: Array<{ from: string; until?: string; fixedAt?: string[] }>;
  applications: Record<string, Array<{ from: string; until?: string; fixedAt?: string[] }>>;
  cvss: { score: number; severity: string; vector: string | null } | null;
  cwe: { id: string; description: string } | null;
  workaround: string | null;
}

/**
 * A bundled component that Erlang/OTP *is* affected by, as opposed to the
 * dismissals. The fix is a version of that component, so it cannot be compared
 * against anything a release carries: which releases are affected comes from
 * the CVE record, and this says what the advisory is about.
 */
interface BundledAffected {
  major: string;
  cve: string;
  component: string;
  ref: string | null;
  apps: string[];
  fixed: string;
}

interface VersionData {
  strs: string[];
  versions: Version[];
  advisories: Advisory[];
  notAffected: NotAffected[];
  cves: Record<string, CveRecord>;
  bundledAffected: BundledAffected[];
  vexMajors: string[];
}

/**
 * Ticket id to the releases whose notes mention it. Those are the releases that
 * introduced the fix; every descendant of one of them contains it too,
 * which the page works out rather than storing. Comes from the patches cache
 * rather than the version data, so the two stay independent of each other.
 */
type Tickets = Record<string, string[]>;

type Severity = "critical" | "high" | "medium" | "low";
type Relation = "sel" | "less" | "gt" | "un";

/** A version with everything derived from it. */
interface VersionNode extends Version {
  major: string;
  branch: string;
  apps: Map<string, string>;
  /** Advisories this release still carries, by CVE id. */
  open: string[];
  /** Advisories whose order against this version is not defined. */
  undetermined: string[];
}

interface Branch {
  id: string;
  base: string;
  rows: VersionNode[];
  /**
   * Branched off something other than the last main-track version of its
   * release. Per the System Principles guide these are branches "established
   * to resolve a particular issue for a specific customer", as opposed to the
   * maintenance branch each release gets when the next one takes over the main
   * track, and they typically become dead ends very quickly.
   */
  oneOff: boolean;
}

interface Major {
  n: string;
  all: VersionNode[];
  trunk: VersionNode[];
  branches: Branch[];
  from: string | null;
  to: string | null;
}

/** The VEX justifications that appear in openvex.table, in plain English. */
const JUSTIFICATION: Record<string, string> = {
  vulnerable_code_not_present: "the vulnerable code is not in the bundled copy",
  vulnerable_code_not_in_execute_path: "the vulnerable code is bundled but never reached",
  component_not_present: "the component is not bundled",
  under_investigation: "still being assessed",
};

const SEVERITY_RANK: Record<Severity, number> = {
  critical: 4,
  high: 3,
  medium: 2,
  low: 1,
};

/**
 * Markup that has already been built and must be interpolated verbatim. Held in
 * a wrapper rather than a bare string so that the one thing you cannot do by
 * accident is emit unescaped text.
 */
class Markup {
  constructor(readonly value: string) {}
  toString(): string {
    return this.value;
  }
}

const escapeHtml = (s: string): string =>
  s.replace(/[&<>"']/g, (c) =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]!)
  );

/** Interpolated values, by type: nested markup verbatim, everything else escaped. */
function interpolate(value: unknown): string {
  if (value === null || value === undefined || value === false) return "";
  if (value instanceof Markup) return value.value;
  if (Array.isArray(value)) return value.map(interpolate).join("");
  return escapeHtml(String(value));
}

/**
 * Builds markup from a template literal, escaping every interpolation unless it
 * is itself markup. Nesting and arrays work as you would expect, so a list of
 * rows is `${rows.map(row)}`.
 */
function html(strings: TemplateStringsArray, ...values: unknown[]): Markup {
  let out = strings[0];
  for (let i = 0; i < values.length; i++) out += interpolate(values[i]) + strings[i + 1];
  return new Markup(out);
}

/** Escape hatch for markup from elsewhere, such as an inline SVG constant. */
const raw = (value: string): Markup => new Markup(value);

/**
 * An outlined shield, so the marker reads as "security" rather than as a
 * generic warning. Takes its colour from the surrounding severity class.
 */
const SHIELD = raw(
  '<svg viewBox="0 0 20 22" width="13" height="14" aria-hidden="true" focusable="false">' +
  '<path d="M10 1.2 2.6 4v7.1c0 4.6 3 8.4 7.4 9.7 4.4-1.3 7.4-5.1 7.4-9.7V4L10 1.2z" ' +
  'fill="none" stroke="currentColor" stroke-width="1.7" stroke-linejoin="round"/>' +
  '<path d="M10 7.2v4.6" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round"/>' +
    '<circle cx="10" cy="15.4" r="1.15" fill="currentColor"/></svg>'
);

/** Branches longer than this collapse to their newest release. */
const COLLAPSE_AT = 4;

/**
 * How many major releases receive security updates. Erlang/OTP supports the
 * last three, counting the current one, per the repository security policy:
 * https://github.com/erlang/otp/blob/master/SECURITY.md
 */
const SUPPORTED_MAJORS = 3;

/** Query parameter holding the selected version, so a view can be linked to. */
const VERSION_PARAM = "v";

// ---------------------------------------------------------------------------
// Page
// ---------------------------------------------------------------------------

class VersionTree {
  private nodes: VersionNode[] = [];
  private byName = new Map<string, VersionNode>();
  private position = new Map<string, number>();
  private majors: Major[] = [];
  private branchesByBase = new Map<string, Branch[]>();
  private appIndex = new Map<string, string[]>();
  private advisoriesByCve = new Map<string, Advisory[]>();
  private bundledByCve = new Map<string, BundledAffected[]>();
  private notAffectedByMajor = new Map<string, NotAffected[]>();
  private appKeys: string[] = [];

  private selected!: string;
  private openMajors = new Set<string>();
  private openBranches = new Set<string>();
  /** What the tree is currently picking out: an application version, or a CVE. */
  private highlight: { app: number } | { cve: string } | { ticket: string } | null = null;
  private compareWith = "";
  /**
   * Advisories the reader has opened. Kept apart from the highlight: hiding the
   * releases an advisory affects should not also fold the advisory away.
   */
  private expandedCves = new Set<string>();
  private hits: Array<{
    label: string;
    meta: string;
    version?: string;
    app?: string;
    cve?: string;
    bundled?: string;
    ticket?: string;
  }> = [];
  /** Index of the result the arrow keys are on, or -1 for none. */
  private activeHit = -1;

  /** Where the generated per-release pages live, from the page's Liquid. */
  private patchesBase: string;

  constructor(
    private data: VersionData,
    private tickets: Tickets,
    private root: HTMLElement
  ) {
    this.patchesBase = root.dataset.patches ?? "/patches/";
    this.build();
    this.selected = this.nodes[this.nodes.length - 1].v;
    this.bind();

    const requested = this.versionFromUrl();
    if (requested) {
      // Arriving on a link: open only the release that version belongs to, and
      // put the legend at the top of the window, so the tree starts just below
      // it rather than the reader having to find a row somewhere down the page.
      this.select(requested);
      this.root.querySelector(".otpv-legend")?.scrollIntoView({ block: "start", behavior: "instant" });
    } else {
      // Otherwise the releases that still receive updates are open to begin with.
      this.majors.slice(0, SUPPORTED_MAJORS).forEach((m) => this.openMajors.add(m.n));
      this.render();
    }
  }

  /** The version named by the query string, if it is one we know. */
  private versionFromUrl(): string | null {
    const raw = new URLSearchParams(window.location.search).get(VERSION_PARAM);
    if (!raw) return null;
    const v = raw.trim().replace(/^(erlang\/)?otp[- ]?(?=\d)/i, "");
    return this.byName.has(v) ? v : null;
  }

  // -- derivation ----------------------------------------------------------

  private build(): void {
    const { strs, versions, advisories } = this.data;

    const byMajor = new Map<string, Advisory[]>();
    const byCve = new Map<string, Advisory[]>();
    for (const a of advisories) {
      const forMajor = byMajor.get(a.major);
      if (forMajor) forMajor.push(a);
      else byMajor.set(a.major, [a]);
      const forCve = byCve.get(a.cve);
      if (forCve) forCve.push(a);
      else byCve.set(a.cve, [a]);
    }
    this.advisoriesByCve = byCve;
    // Advisories whose CVE record describes releases directly. Every advisory
    // is considered, not only those openvex mentions, and they apply to every
    // release in the tree rather than to one major.
    const byRelease = Object.entries(this.data.cves).filter(([, c]) => c.releases.length);

    this.nodes = versions.map((r) => {
      const apps = new Map<string, string>();
      for (const i of r.c.concat(r.s)) {
        const s = strs[i];
        const dash = s.lastIndexOf("-");
        apps.set(s.slice(0, dash), s.slice(dash + 1));
      }
      const major = r.v.split(".")[0];
      const node: VersionNode = {
        ...r,
        major,
        branch: branchOf(r.v),
        apps,
        open: [],
        undetermined: [],
      };
      // Where the CVE record says which releases are affected, it is the better
      // answer: it reaches back to the release that introduced the flaw, while
      // openvex only describes the majors still being updated.
      const decided = new Set<string>();
      for (const [cve, record] of byRelease) {
        decided.add(cve);
        if (inAnyRange(r.v, record.releases)) node.open.push(cve);
      }

      // Otherwise a release still carries an advisory when its version of the
      // affected application is ordered below the version that fixed it. Where
      // the two have no order, nothing can be concluded either way.
      for (const a of byMajor.get(major) ?? []) {
        if (decided.has(a.cve)) continue;
        const have = apps.get(a.app);
        if (have === undefined) continue;
        const toFix = compare(have, a.fixed);
        const fromIntro = compare(have, a.introduced);
        if (toFix !== null && toFix >= 0) continue;
        if (toFix !== null && toFix < 0 && fromIntro !== null && fromIntro >= 0) {
          node.open.push(a.cve);
          decided.add(a.cve);
        } else if (!node.undetermined.includes(a.cve)) {
          node.undetermined.push(a.cve);
        }
      }
      return node;
    });

    this.nodes.forEach((n, i) => {
      this.byName.set(n.v, n);
      this.position.set(n.v, i);
      for (const ix of n.c.concat(n.s)) {
        const key = strs[ix];
        const list = this.appIndex.get(key);
        if (list) list.push(n.v);
        else this.appIndex.set(key, [n.v]);
      }
    });
    this.appKeys = [...this.appIndex.keys()].sort();

    for (const b of this.data.bundledAffected ?? []) {
      const list = this.bundledByCve.get(b.cve);
      if (list) list.push(b);
      else this.bundledByCve.set(b.cve, [b]);
    }

    // An assessment can be overtaken by events: openvex.table still carries
    // CVE-2025-58050 against the bundled PCRE2 as "under investigation", while
    // the same CVE already has a resolved advisory against erts with a fix
    // version. The resolved advisory wins, or the page would report the same
    // vulnerability as both open and unassessed.
    const resolved = new Set(this.data.advisories.map((a) => `${a.cve}@${a.major}`));
    for (const n of this.data.notAffected) {
      if (resolved.has(`${n.id}@${n.major}`)) continue;
      const list = this.notAffectedByMajor.get(n.major);
      if (list) list.push(n);
      else this.notAffectedByMajor.set(n.major, [n]);
    }

    // Group into majors, each with its trunk run and the branches off it.
    const majorMap = new Map<string, Major>();
    const branchMap = new Map<string, Branch>();
    for (const n of this.nodes) {
      let m = majorMap.get(n.major);
      if (!m) {
        m = { n: n.major, all: [], trunk: [], branches: [], from: null, to: null };
        majorMap.set(n.major, m);
        this.majors.push(m);
      }
      m.all.push(n);
      if (n.branch === "trunk") {
        m.trunk.push(n);
      } else {
        let b = branchMap.get(n.branch);
        if (!b) {
          b = { id: n.branch, base: baseOf(n.branch), rows: [], oneOff: false };
          branchMap.set(n.branch, b);
          m.branches.push(b);
        }
        b.rows.push(n);
      }
    }

    this.majors.sort((a, b) => Number(b.n) - Number(a.n));
    for (const m of this.majors) {
      const dates = m.all.map((n) => n.d).filter((d): d is string => d !== null).sort();
      m.from = dates[0] ?? null;
      m.to = dates[dates.length - 1] ?? null;
      // Newest first, matching how the tree is read.
      m.trunk.reverse();
      m.branches.forEach((b) => b.rows.reverse());

      // A release's maintenance branch is the one cut from its last main-track
      // version, when the next release took over. Anything else is a one-off.
      const lastOnMainTrack = m.trunk[0]?.v;
      for (const b of m.branches) {
        const base = this.byName.get(b.base);
        b.oneOff = !base || base.branch !== "trunk" || b.base !== lastOnMainTrack;
      }
    }
    for (const b of branchMap.values()) {
      const list = this.branchesByBase.get(b.base);
      if (list) list.push(b);
      else this.branchesByBase.set(b.base, [b]);
    }
  }

  /** The version immediately before this one on its own line. */
  private predecessor(n: VersionNode): VersionNode | null {
    const major = this.majors.find((m) => m.n === n.major)!;
    const line =
      n.branch === "trunk"
        ? major.trunk
        : major.branches.find((b) => b.id === n.branch)!.rows;
    const i = line.indexOf(n);
    if (i < line.length - 1) return line[i + 1];
    if (n.branch !== "trunk") return this.byName.get(baseOf(n.branch)) ?? null;
    const older = this.majors[this.majors.indexOf(major) + 1];
    return older ? older.trunk[0] : null;
  }

  private relation(v: string): Relation {
    if (v === this.selected) return "sel";
    const c = compare(v, this.selected);
    if (c === null) return "un";
    return c < 0 ? "less" : "gt";
  }

  private worstSeverity(cves: string[]): Severity | null {
    let worst: Severity | null = null;
    for (const cve of cves) {
      const severity = this.data.cves[cve]?.severity;
      if (severity && (!worst || SEVERITY_RANK[severity] > SEVERITY_RANK[worst])) {
        worst = severity;
      }
    }
    return worst;
  }

  private matchesHighlight(n: VersionNode): boolean {
    const h = this.highlight;
    if (!h) return false;
    if ("app" in h) return n.c.includes(h.app) || n.s.includes(h.app);
    if ("cve" in h) return n.open.includes(h.cve);
    return (this.tickets[h.ticket] ?? []).includes(n.v);
  }

  /**
   * What the highlight is picking out, in words, since the tree alone does not
   * say why those rows and not others.
   */
  private highlightSummary(): Markup | null {
    const h = this.highlight;
    if (!h) return null;
    const matched = this.nodes.filter((n) => this.matchesHighlight(n)).length;
    if ("app" in h) return html`<b>${this.data.strs[h.app]}</b> is in ${matched} releases`;
    if ("cve" in h) return html`<b>${h.cve}</b> is still open in ${matched} releases`;
    const releases = this.tickets[h.ticket] ?? [];
    const listed = releases.slice(0, 4).join(", ");
    const rest = releases.length > 4 ? ` and ${releases.length - 4} more` : "";
    return html`<b>${h.ticket}</b> was fixed in ${listed}${rest}`;
  }

  /** Names the release to move to, which is the actionable half of an advisory. */
  private fixedInHtml(cve: string, from: VersionNode): Markup | string {
    const fix = this.fixedIn(cve, from);
    if (!fix) return "";
    return fix.onThisLine
      ? html`<span class="otpv-fix">first fixed in <b>Erlang/OTP ${fix.version}</b>, which is a
          descendant of ${from.v}</span>`
      : html`<span class="otpv-fix">fixed in <b>Erlang/OTP ${fix.version}</b>, which has no order against
          ${from.v} &mdash; moving there is not guaranteed to keep what you have</span>`;
  }

  /**
   * The applications an advisory concerns. Openvex files it per major and per
   * application, so a release outside those majors falls back to whatever the
   * advisory names anywhere.
   */
  private appsFor(cve: string, major: string): string[] {
    const all = this.advisoriesByCve.get(cve) ?? [];
    const here = all.filter((a) => a.major === major);
    const named = [...new Set((here.length ? here : all).map((a) => a.app))];
    // openvex only covers the majors still updated, so an advisory it has not
    // caught up with falls back to what the CVE record names.
    if (named.length) return named;
    const fromRecord = Object.keys(this.data.cves[cve]?.applications ?? {});
    if (fromRecord.length) return fromRecord;
    // Reached through a bundled component: name the application that ships it.
    return [...new Set((this.bundledByCve.get(cve) ?? []).flatMap((b) => b.apps))];
  }

  /**
   * The version of an application that fixes an advisory, for a release
   * carrying `have`. Taken from the CVE record where it says so, since openvex
   * only describes the majors it covers; null when this version is not affected.
   */
  private applicationFix(cve: string, app: string, have: string): string | null {
    for (const range of this.data.cves[cve]?.applications?.[app] ?? []) {
      const since = compare(have, range.from);
      if (since === null || since < 0) continue;
      if (range.until) {
        const c = compare(have, range.until);
        if (c !== null && c < 0) return range.until;
        continue;
      }
      // An open range is fixed at one point per maintenance line. If none of
      // them is at or below what is installed, the relevant fix is the earliest
      // ordered above it.
      const fixes = range.fixedAt ?? [];
      if (fixes.some((f) => { const c = compare(have, f); return c !== null && c >= 0; })) continue;
      const ahead = fixes
        .filter((f) => { const c = compare(have, f); return c !== null && c < 0; })
        .sort((a, b) => compare(a, b) ?? 0);
      if (ahead.length) return ahead[0];
    }
    return null;
  }

  /** Every release still carrying this advisory. */
  private affectedBy(cve: string): VersionNode[] {
    return this.nodes.filter((n) => n.open.includes(cve));
  }

  /**
   * The first release that carries the fix and is a descendant of `from`, so
   * upgrading to it is guaranteed to include everything already in hand. Falls
   * back to the earliest release carrying the fix at all, flagged as being off
   * this line, when no descendant has it.
   */
  private fixedIn(cve: string, from: VersionNode): { version: string; onThisLine: boolean } | null {
    const record = this.data.cves[cve];
    // Where the CVE record names the releases that carry the fix, use those:
    // they are stated rather than inferred, and they cover the lines openvex
    // says nothing about.
    const stated = (record?.releases ?? []).flatMap((r) => r.fixedAt ?? (r.until ? [r.until] : []));
    const known = stated.filter((v) => this.byName.has(v));
    const carriesFix = known.length
      ? known.map((v) => this.byName.get(v)!).sort((x, y) => this.position.get(x.v)! - this.position.get(y.v)!)
      : this.nodes.filter((n) =>
          (this.advisoriesByCve.get(cve) ?? []).some((a) => {
            const have = n.apps.get(a.app);
            if (have === undefined) return false;
            const c = compare(have, a.fixed);
            return c !== null && c >= 0;
          })
        );
    const descendant = carriesFix.find((n) => {
      const c = compare(from.v, n.v);
      return c !== null && c < 0;
    });
    if (descendant) return { version: descendant.v, onThisLine: true };
    return carriesFix.length ? { version: carriesFix[0].v, onThisLine: false } : null;
  }

  /**
   * Picks something out across the tree, opening whatever is needed to make the
   * matches visible.
   */
  private setHighlight(h: { app: number } | { cve: string } | { ticket: string } | null): void {
    this.highlight = h;
    if (h) {
      for (const n of this.nodes) {
        if (!this.matchesHighlight(n)) continue;
        this.openMajors.add(n.major);
        let branch = n.branch;
        while (branch !== "trunk") {
          this.openBranches.add(branch);
          const base = this.byName.get(baseOf(branch));
          if (!base) break;
          branch = base.branch;
        }
      }
    }
  }

  /** The branch a version sits on, or undefined for the main track. */
  private branchOf(n: VersionNode): Branch | undefined {
    return this.majors.find((m) => m.n === n.major)?.branches.find((b) => b.id === n.branch);
  }

  /** Whether a major release still receives security updates. */
  private isSupported(major: string): boolean {
    return Number(major) > Number(this.majors[0].n) - SUPPORTED_MAJORS;
  }

  private el<T extends HTMLElement = HTMLElement>(id: string): T {
    return this.root.querySelector<T>("#" + id)!;
  }

  // -- rendering -----------------------------------------------------------

  private render(): void {
    this.renderTree();
    this.renderDetail();
    // Revealed once there is something to show, so the shell is not left
    // half-rendered while the data loads or if it never arrives.
    this.root.querySelectorAll<HTMLElement>(".otpv-legend, .otpv-cols").forEach((e) => (e.hidden = false));
    // The detail panel runs to a few thousand characters; announcing all of it
    // on every selection would be unusable, so only the change is announced.
    const announce = this.root.querySelector("#otpv-announce");
    if (announce) announce.textContent = `Selected Erlang/OTP ${this.selected}`;
  }

  private rowHtml(n: VersionNode, isHead = false): Markup {
    const highlighted = this.matchesHighlight(n);
    const changed = n.c.map((i) => this.data.strs[i]);
    const shown =
      changed.slice(0, 4).join("  ") + (changed.length > 4 ? "  +" + (changed.length - 4) : "");
    const severity = this.worstSeverity(n.open);
    const classes = [
      this.relation(n.v),
      highlighted ? "match" : "",
      isHead ? "head" : "",
      // 17.0 is the root of the whole tree, so the main track stops there.
      n === this.nodes[0] ? "root" : "",
    ].filter(Boolean);
    const openCount = `${n.open.length} open advisor${n.open.length === 1 ? "y" : "ies"}`;
    const warning = severity
      ? html` title="${openCount}, worst severity ${severity}"`
      : "";

    return html`
      <div class="otpv-row ${classes.join(" ")}" data-v="${n.v}">
        <span class="otpv-spine"><span class="otpv-dot"></span></span>
        <button class="otpv-row-btn" type="button">
          <span class="otpv-v">${n.v}</span>
          <span class="otpv-warn ${severity ?? ""}"${warning}>${severity ? SHIELD : ""}</span>
          <span class="otpv-apps">${changed.length ? shown : raw("&mdash;")}</span>
          <span class="otpv-date">${n.d ?? ""}</span>
        </button>
      </div>`;
  }

  /**
   * A branch is drawn above the version it grew out of, since every release on
   * it is newer than that version. Branches off branches nest the same way.
   */
  private branchHtml(b: Branch): Markup {
    const holdsSelection = b.rows.some((r) => r.v === this.selected);
    const collapsible = b.rows.length > COLLAPSE_AT;
    const collapsed = collapsible && !this.openBranches.has(b.id) && !holdsSelection;
    const shown = collapsed ? b.rows.slice(0, 1) : b.rows;
    const hidden = b.rows.length - shown.length;

    // Rows run newest first, so the branch sits above the version it came from.
    const rows = shown.map((r, i) => this.rowsWithBranches(r, i === 0));

    // While releases are hidden they get a node of their own on the line, so the
    // branch reads as continuing past what is shown. Once expanded there is
    // nothing to stand in for, so the control moves onto the branch label.
    const standIn = collapsed
      ? html`
        <div class="otpv-more">
          <span class="otpv-spine"><span class="otpv-dot"></span></span>
          <button class="otpv-more-btn" type="button" data-branch="${b.id}">
            show ${hidden} older release${hidden === 1 ? "" : "s"}
          </button>
        </div>`
      : "";
    const toggle =
      collapsible && !collapsed
        ? html` <button class="otpv-more-btn" type="button" data-branch="${b.id}">show fewer</button>`
        : "";
    const whyOneOff =
      "Cut from a version other than the last on the main track, to resolve a " +
      "particular issue rather than to maintain the release.";
    const kind = b.oneOff
      ? html`<span class="otpv-oneoff" title="${whyOneOff}">one-off</span>`
      : "";

    return html`
      <div class="otpv-branch${b.oneOff ? " one-off" : ""}">
        <div class="otpv-rows">
          ${rows}${standIn}
        </div>
        <div class="otpv-branch-head">
          <span class="otpv-spine"><span class="otpv-elbow"></span></span>
          <span class="otpv-branch-label">
            branch <b>${b.id}</b> &mdash;
            ${b.rows.length} release${b.rows.length === 1 ? "" : "s"} off <b>${b.base}</b>
            ${kind}${toggle}
          </span>
        </div>
      </div>`;
  }

  private rowsWithBranches(n: VersionNode, isHead = false): Markup {
    const spurs = (this.branchesByBase.get(n.v) ?? []).map((b) => this.branchHtml(b));
    return html`${spurs}${this.rowHtml(n, isHead)}`;
  }

  private renderTree(): void {
    this.root
      .querySelectorAll(".otpv-relv")
      .forEach((e) => (e.textContent = this.selected));

    const v = "Erlang/OTP " + this.selected;
    this.el("otpv-tip-self").textContent =
      `${v} — the version every other version on this page is compared against. ` +
      "Select any version in the tree to compare against that one instead.";
    this.el("otpv-tip-less").textContent =
      `The versions ${v} is built on. Everything released in them is also in it, ` +
      "so moving up from one of them loses nothing.";
    this.el("otpv-tip-gt").textContent =
      `The versions built on ${v}. Each one contains everything in it, so moving ` +
      "to one of them is a strict step forward.";
    this.el("otpv-tip-un").textContent =
      `Neither these nor ${v} is an ancestor of the other, so nothing follows about ` +
      "what either contains — a fix present in one may simply not exist in the other.";

    const first = this.nodes[0].d;
    const last = this.nodes[this.nodes.length - 1].d;
    const span = first && last ? ` &middot; ${first.slice(0, 4)}&ndash;${last.slice(0, 4)}` : "";
    const branches = [...this.branchesByBase.values()].flat().length;
    this.el("otpv-count").innerHTML = String(
      this.highlightSummary() ??
        html`${this.nodes.length} releases &middot; ${this.majors.length} major versions &middot;
          ${branches} branches${raw(span)}`
    );

    this.el("otpv-tree").innerHTML = String(html`${this.majors.map((m, i) => {
        const open = this.openMajors.has(m.n);
        const seg: Record<Relation, number> = { sel: 0, less: 0, gt: 0, un: 0 };
        m.all.forEach((n) => seg[this.relation(n.v)]++);
        const bar = (["sel", "gt", "less", "un"] as Relation[])
          .filter((k) => seg[k])
          .map((k) => {
            const width = Math.max(3, Math.round((seg[k] / m.all.length) * 96));
            return html`<i class="otpv-seg-${k}" style="width:${width}px"></i>`;
          });
        const from = m.from?.slice(0, 4);
        const to = m.to?.slice(0, 4);
        const years = from && to ? ` \u00b7 ${from}${to === from ? "" : "\u2013" + to}` : "";
        const branches = m.branches.length
          ? ` \u00b7 ${m.branches.length} branch${m.branches.length > 1 ? "es" : ""}`
          : "";
          const head = html`
        <button class="otpv-major-btn" type="button" data-major="${m.n}"
                aria-expanded="${open ? "true" : "false"}">
          <span class="otpv-spine${i === this.majors.length - 1 ? " last" : ""}">
            <span class="otpv-node"></span>
          </span>
          <span class="otpv-major-name">
            Erlang/OTP ${m.n}
            <small>${m.all.length} release${m.all.length === 1 ? "" : "s"}${years}${branches}</small>
            ${this.isSupported(m.n) ? "" : html`<span class="otpv-chip eol">no longer supported</span>`}
          </span>
          <span class="otpv-relbar">
            ${bar}<span class="otpv-caret">${open ? raw("&minus;") : "+"}</span>
          </span>
        </button>`;
        const body = open
          ? html`
        <div class="otpv-rows">
          ${m.trunk.map((n) => this.rowsWithBranches(n))}
        </div>`
          : "";
        return html`
      <div class="otpv-major${open ? " open" : ""}${seg.sel ? " on" : ""}">${head}${body}
      </div>`;
    })}`);
  }

  private renderDetail(): void {
    const n = this.byName.get(this.selected)!;
    const previous = this.predecessor(n);
    const changed = n.c.map((i) => this.data.strs[i]).sort();
    const unchanged = n.s.map((i) => this.data.strs[i]).sort();
    const tag = "OTP-" + n.v;
    const severity = this.worstSeverity(n.open);
    const newest = this.nodes[this.nodes.length - 1];

    const appRow = (s: string, dim: boolean): Markup => {
      const dash = s.lastIndexOf("-");
      const name = s.slice(0, dash);
      const version = s.slice(dash + 1);
      const before = previous?.apps.get(name);
      const was = !dim && before && before !== version ? html`<s>${before}</s> ` : "";
      return html`<tr>
        <td>${name}</td>
        <td class="otpv-av">${was}${dim ? version : html`<b>${version}</b>`}</td>
      </tr>`;
    };

    const branch = this.branchOf(n);
    const onBranch =
      n.branch === "trunk"
        ? html`On the <b>main track</b>, where every release was the newest Erlang/OTP at the time.`
        : branch?.oneOff
          ? html`On branch <b>${n.branch}</b>, based on <b>${baseOf(n.branch)}</b> &mdash; a one-off branch,
              cut to resolve a particular issue rather than to maintain the release.`
          : html`On branch <b>${n.branch}</b>, based on <b>${baseOf(n.branch)}</b> &mdash; maintenance
              released after Erlang/OTP ${Number(n.major) + 1} had taken over the main track.`;

    this.el("otpv-detail").innerHTML = String(html`
      <div class="card-header">
        <h5 class="otpv-selected">Erlang/OTP ${n.v}</h5>
        <div class="otpv-meta">
          <span class="otpv-chip plain">${n.d ? "tagged " + n.d : "date unrecorded"}</span>
          <span class="otpv-chip plain">
            ${n.c.length} of ${n.c.length + n.s.length} applications changed
          </span>
          ${severity
            ? html`<span class="otpv-chip with-icon sev-${severity}"
                >${SHIELD}${n.open.length} open advisor${n.open.length === 1 ? "y" : "ies"}</span
              >`
            : ""}
          ${this.isSupported(n.major) ? "" : html`<span class="otpv-chip eol">no longer supported</span>`}
        </div>
        <p class="text-muted mb-0">${onBranch}</p>
      </div>

      <div class="otpv-sect">
        <h5 class="border-bottom">Security</h5>
        ${this.securityHtml(n)}
      </div>

      <div class="otpv-sect">
        <h5 class="border-bottom">Is it in there?</h5>
        <input id="otpv-cmp" class="form-control" type="search" spellcheck="false" autocomplete="off"
          value="${this.compareWith}" placeholder="Compare with e.g. ${newest.v}">
        ${this.verdictHtml()}
      </div>

      <div class="otpv-sect">
        <h5 class="border-bottom">Upgrading</h5>
        <p>${this.upgradeHtml(n)}</p>
      </div>

      <div class="otpv-sect">
        <h5 class="border-bottom">Applications changed here &mdash; ${changed.length}</h5>
        ${changed.length
          ? html`<table class="table table-sm otpv-apps-list">${changed.map((a) => appRow(a, false))}</table>`
          : html`<p>No application changed in this version.</p>`}
      </div>

      <div class="otpv-sect">
        <details>
          <summary>Unchanged applications &mdash; ${unchanged.length}</summary>
          <table class="table table-sm otpv-apps-list muted">${unchanged.map((a) => appRow(a, true))}</table>
        </details>
      </div>

      <div class="otpv-sect">
        <h5 class="border-bottom">Release</h5>
        <div class="otpv-links">
          <a class="btn btn-sm btn-outline-primary" href="${this.patchesBase + tag}">
            Release notes &amp; downloads
          </a>
          <a class="btn btn-sm btn-outline-secondary"
            href="https://github.com/erlang/otp/releases/tag/${tag}">Git tag</a>
        </div>
        <p class="text-muted mt-3 mb-0">The release page lists the source archive, documentation and Windows
          installers for this version.</p>
      </div>
    `);

    const input = this.el<HTMLInputElement>("otpv-cmp");
    input.addEventListener("input", () => {
      this.compareWith = input.value;
      const caret = input.selectionStart;
      this.renderDetail();
      const next = this.el<HTMLInputElement>("otpv-cmp");
      next.focus();
      next.setSelectionRange(caret, caret);
    });
  }

  private securityHtml(n: VersionNode): Markup {
    if (!Object.keys(this.data.cves).length) {
      return html`<p class="text-muted">Advisory data is unavailable.</p>`;
    }
    const supported = this.majors
      .slice(0, SUPPORTED_MAJORS)
      .map((m) => m.n)
      .reverse()
      .join(", ");
    const unsupported = this.isSupported(n.major)
      ? ""
      : html`<div class="otpv-eol-note">
          <b>Erlang/OTP ${n.major} no longer receives security updates.</b>
          <p>Erlang/OTP supports the last ${SUPPORTED_MAJORS} releases, currently ${supported}. Newly
            discovered vulnerabilities will not be fixed here.</p>
        </div>`;

    const showing = this.highlight && "cve" in this.highlight ? this.highlight.cve : null;
    const advisories = n.open.length
      ? [...n.open]
          .sort(
            (a, b) =>
              (SEVERITY_RANK[this.data.cves[b]?.severity!] ?? 0) -
              (SEVERITY_RANK[this.data.cves[a]?.severity!] ?? 0)
          )
          .map((cve) => {
            const record = this.data.cves[cve];
            const applications = this.appsFor(cve, n.major);
            // What this release carries, against the version that fixes it. The
            // CVE record answers for any release; openvex only for the majors
            // it covers, and only once its bot has caught up.
            const versions = applications
              .map((app) => {
                const have = n.apps.get(app);
                if (have === undefined) return null;
                const openvex = (this.advisoriesByCve.get(cve) ?? []).find(
                  (e) => e.app === app && e.major === n.major
                );
                const fixed = this.applicationFix(cve, app, have) ?? openvex?.fixed;
                return fixed ? { app, have, fixed } : null;
              })
              .filter((v): v is { app: string; have: string; fixed: string } => v !== null);
            return html`
              <details class="otpv-cve"${this.expandedCves.has(cve) ? raw(" open") : ""}>
                <summary class="otpv-cve-top">
                  <span class="otpv-chip sev-${record?.severity}">${record?.severity}${record?.cvss ? ` ${record.cvss.score}` : ""}</span>
                  ${applications.length ? html`<span class="otpv-cve-app">${applications.join(", ")}</span>` : ""}
                  <span class="otpv-cve-title">${record?.summary ?? cve}</span>
                </summary>
                <span class="otpv-fix"
                  >${cve}${record?.cwe ? ` \u00b7 ${record.cwe.id} ${record.cwe.description}` : ""}</span
                >
                ${versions.map(
                  (e) => html`<span class="otpv-fix">${e.app} ${e.have} &lt; <b>${e.fixed}</b> &mdash; not fixed here</span>`
                )}
                ${(this.bundledByCve.get(cve) ?? []).map(
                  (b) => html`<span class="otpv-fix">through bundled ${b.component}, fixed in
                    <b>${b.fixed}</b> of that component</span>`
                )}
                ${record?.cvss?.vector ? html`<span class="otpv-fix">${record.cvss.vector}</span>` : ""}
                ${this.fixedInHtml(cve, n)}
                ${record?.workaround
                  ? html`
                    <details class="otpv-workaround">
                      <summary>Workaround</summary>
                      <p>${record.workaround}</p>
                    </details>`
                  : ""}
                <div class="otpv-links">
                  <button class="btn btn-sm btn-outline-secondary" type="button" data-cve="${cve}"
                          aria-pressed="${showing === cve ? "true" : "false"}"
                  >${showing === cve ? "Hide affected versions" : "Show affected versions"}</button>
                  ${record?.url
                    ? html`<a class="btn btn-sm btn-outline-primary" href="${record.url}">${record.ghsa ?? "Advisory"}</a>`
                    : ""}
                </div>
              </details>`;
          })
      : html`
        <div class="otpv-safe">
          <b>No open advisories</b>
          <p>No advisory names this release, and none of the application versions it carries is below the
            version that fixed one.</p>
        </div>`;


    // Neither source places these: the CVE record does not say which releases
    // are affected and openvex does not list them at all. Kept apart from the
    // advisories above, because not knowing is not the same as being affected.
    const unplaced = Object.entries(this.data.cves).filter(
      ([cve, record]) => !record.releases.length && !this.advisoriesByCve.has(cve)
    );
    const unplacedHtml = unplaced.length
      ? html`
        <details class="otpv-bundled">
          <summary>Advisories that could not be placed &mdash; ${unplaced.length}</summary>
          <p class="text-muted">Neither the CVE record nor <code>openvex.table</code> says which releases
            these affect, so whether this one is among them is unknown.</p>
          <ul class="otpv-bundled-list">
            ${unplaced.map(
              ([cve, record]) => html`<li>
                <span class="otpv-bundled-id">${cve}</span>
                <span class="otpv-chip plain">${record.severity ?? "unrated"}</span>
                <span class="otpv-bundled-why">${record.summary ?? ""}</span>
              </li>`
            )}
          </ul>
        </details>`
      : "";

    const undetermined = n.undetermined.length
      ? html`<p class="text-muted">${n.undetermined.length} advisor${n.undetermined.length === 1
            ? "y has"
            : "ies have"} no defined order against this version, so whether it is affected cannot be
          determined.</p>`
      : "";

    return html`${unsupported}${advisories}${undetermined}${unplacedHtml}${this.bundledHtml(n.major)}`;
  }

  /**
   * Vulnerabilities reported against a bundled component. Every one of these is
   * an assessment of the major release rather than of a single version, so it
   * is kept apart from the advisories above and labelled as such.
   */
  private bundledHtml(major: string): Markup | string {
    const list = this.notAffectedByMajor.get(major);
    if (!list || list.length === 0) return "";
    const rows = [...list]
      .sort((a, b) => a.component.localeCompare(b.component) || a.id.localeCompare(b.id))
      .map((a) => {
        const investigating = a.justification === "under_investigation";
        return html`<li>
          <span class="otpv-bundled-id">${a.id}</span>
          <span class="otpv-chip plain">${investigating ? "under investigation" : "not affected"}</span>
          <span class="otpv-bundled-why"
            >${a.component}${a.apps.length ? ` bundled by ${a.apps.join(", ")}` : ""} &mdash;
            ${JUSTIFICATION[a.justification] ?? a.justification}</span
          >
        </li>`;
      });
    return html`<details class="otpv-bundled">
      <summary>Bundled components &mdash; ${list.length}</summary>
      <p class="text-muted">Reported against a third-party component that Erlang/OTP ships, and assessed for
        Erlang/OTP ${major} as a whole rather than for a single version.</p>
      <ul class="otpv-bundled-list">${rows}</ul>
    </details>`;
  }

  private verdictHtml(): Markup | string {
    const other = this.compareWith.trim().replace(/^(erlang\/)?otp[- ]?(?=\d)/i, "");
    if (!other) return "";
    if (!/^\d+(\.\d+)*$/.test(other)) {
      return html`<div class="otpv-verdict">
        <b>${other}</b>
        <p>Not a version number. Try something like <code>27.3.4.6</code>.</p>
      </div>`;
    }
    const known = this.byName.has(other)
      ? ""
      : html` <span class="otpv-chip plain">never released</span>`;
    const c = compare(this.selected, other);
    if (c === 0) {
      return html`<div class="otpv-verdict"><b>${this.selected} = ${other}</b><p>The same version.</p></div>`;
    }
    if (c === null) {
      return html`<div class="otpv-verdict un">
        <b>${this.selected} ? ${other}</b>${known}
        <p>No order. Neither is an ancestor of the other, so nothing follows about what either one contains
          &mdash; a fix present in one may simply not exist in the other.</p>
      </div>`;
    }
    if (c < 0) {
      return html`<div class="otpv-verdict gt">
        <b>${this.selected} &lt; ${other}</b>${known}
        <p><b>${other}</b> is a descendant. Everything in ${this.selected} is also in it, so it is a safe move
          from here.</p>
      </div>`;
    }
    return html`<div class="otpv-verdict less">
      <b>${this.selected} &gt; ${other}</b>${known}
      <p><b>${other}</b> is an ancestor. You already have everything it contains.</p>
    </div>`;
  }

  private upgradeHtml(n: VersionNode): Markup {
    const major = this.majors.find((m) => m.n === n.major)!;
    const line = n.branch === "trunk" ? major.trunk : major.branches.find((b) => b.id === n.branch)!.rows;
    const head = line[0];
    const newest = this.nodes[this.nodes.length - 1];
    if (head.v !== n.v) {
      return html`<b>${head.v}</b> is the newest version on this line and is a descendant of ${n.v}
        &mdash; the safe upgrade without leaving the branch.`;
    }
    if (n.v !== newest.v) {
      return html`This is the newest version on its line. Moving to <b>${newest.v}</b> crosses to the main
        track, where the order against ${n.v} is
        <b>${compare(n.v, newest.v) === null ? "undefined" : "defined"}</b>.`;
    }
    return html`The newest Erlang/OTP version. Everything else in the tree is an ancestor of it or has no
      order against it.`;
  }

  // -- interaction ---------------------------------------------------------

  private select(v: string, opts: { scroll?: ScrollBehavior; push?: boolean } = {}): void {
    const n = this.byName.get(v);
    if (!n) return;
    this.selected = v;
    this.openMajors.add(n.major);
    // A version on a branch off a branch is only rendered once every branch
    // above it is open too, since each hangs off a row of its parent.
    let branch = n.branch;
    while (branch !== "trunk") {
      this.openBranches.add(branch);
      const base = this.byName.get(baseOf(branch));
      if (!base) break;
      branch = base.branch;
    }
    this.render();

    if (opts.push) {
      const url = new URL(window.location.href);
      url.searchParams.set(VERSION_PARAM, v);
      window.history.pushState({ [VERSION_PARAM]: v }, "", url);
    }
    if (opts.scroll) {
      // The site sets scroll-behavior: smooth, which "auto" would inherit;
      // arriving on a link should land straight on the version instead.
      this.root
        .querySelector(`.otpv-row[data-v="${CSS.escape(v)}"]`)
        ?.scrollIntoView({ block: "center", behavior: opts.scroll });
    }
  }

  private bind(): void {
    this.el("otpv-tree").addEventListener("click", (e) => {
      const target = e.target as HTMLElement;
      const major = target.closest<HTMLElement>(".otpv-major-btn");
      if (major) {
        const n = major.dataset.major!;
        this.openMajors.has(n) ? this.openMajors.delete(n) : this.openMajors.add(n);
        this.renderTree();
        return;
      }
      const more = target.closest<HTMLElement>(".otpv-more-btn");
      if (more) {
        const id = more.dataset.branch!;
        this.openBranches.has(id) ? this.openBranches.delete(id) : this.openBranches.add(id);
        this.renderTree();
        return;
      }
      const row = target.closest<HTMLElement>(".otpv-row");
      if (row) this.select(row.dataset.v!, { push: true });
    });

    this.el("otpv-detail").addEventListener("click", (e) => {
      const button = (e.target as HTMLElement).closest<HTMLElement>("[data-cve]");
      if (!button) return;
      const cve = button.dataset.cve!;
      const showing = this.highlight && "cve" in this.highlight && this.highlight.cve === cve;
      this.setHighlight(showing ? null : { cve });
      // The advisory stays open either way; the button is inside it.
      this.expandedCves.add(cve);
      this.render();
    });

    // `toggle` does not bubble, so it is caught on the way down.
    this.el("otpv-detail").addEventListener(
      "toggle",
      (e) => {
        const details = e.target as HTMLDetailsElement;
        if (!details.classList?.contains("otpv-cve")) return;
        const cve = details.querySelector<HTMLElement>("[data-cve]")?.dataset.cve;
        if (!cve) return;
        if (details.open) this.expandedCves.add(cve);
        else this.expandedCves.delete(cve);
      },
      true
    );

    this.el("otpv-expand").addEventListener("click", () => {
      this.majors.forEach((m) => this.openMajors.add(m.n));
      this.renderTree();
    });
    this.el("otpv-collapse").addEventListener("click", () => {
      this.openMajors.clear();
      this.renderTree();
    });

    const search = this.el<HTMLInputElement>("otpv-q");
    const results = this.el("otpv-hits");
    const runSearch = () => {
      // "OTP-28.5" is a release, "OTP-19180" is a ticket. Only strip the prefix
      // when what follows looks like a version, which is at most two digits
      // before the first dot; ticket numbers are far longer.
      const typed = search.value.trim().toLowerCase();
      const q = typed.replace(/^(erlang\/)?otp[- ]?(?=\d{1,2}(\.|$))/, "");
      if (!q) {
        results.innerHTML = "";
        if (this.highlight) {
          this.setHighlight(null);
          this.renderTree();
        }
        return;
      }
      const hits: typeof this.hits = [];
      for (const n of this.nodes) {
        if (n.v.startsWith(q)) hits.push({ label: "Erlang/OTP " + n.v, meta: n.d ?? "", version: n.v });
      }
      hits.sort((a, b) => this.position.get(b.version!)! - this.position.get(a.version!)!);
      hits.length = Math.min(hits.length, 7);
      for (const key of this.appKeys) {
        if (hits.length >= 14) break;
        if (key.toLowerCase().startsWith(q)) {
          hits.push({ label: key, meta: `${this.appIndex.get(key)!.length} Erlang/OTP versions`, app: key });
        }
      }
      for (const [id, a] of Object.entries(this.data.cves)) {
        if (hits.length >= 20) break;
        const cve = id.toLowerCase();
        const ghsa = (a.ghsa ?? "").toLowerCase();
        const summary = a.summary ?? "";
        // An id matches from the start freely, but only as a substring once the
        // query is long enough to mean something: a GHSA id is a random group
        // of letters, so "pr" is inside plenty of them. The wording is searched
        // from three characters in, for the same reason — otherwise "pr" turns
        // up everything that says "Predictable", which the result never shows.
        const found =
          cve.startsWith(typed) ||
          ghsa.startsWith(typed) ||
          (typed.length >= 4 && (cve.includes(typed) || ghsa.includes(typed))) ||
          (typed.length >= 3 && summary.toLowerCase().includes(typed));
        if (found && !hits.some((h) => h.cve === id)) {
          const short = summary.length > 58 ? summary.slice(0, 58) + "\u2026" : summary;
          hits.push({ label: id, meta: `${a.severity ?? ""} · ${short}`, cve: id });
        }
      }
      // "#8699" is how a pull request or issue is written on github, so accept
      // that for the PR- and GH- ids as well as their spelled-out form.
      const issue = /^#(\d+)$/.exec(typed);
      for (const id of Object.keys(this.tickets)) {
        if (hits.length >= 24) break;
        const lower = id.toLowerCase();
        const matches = issue
          ? /^(pr|gh)-/.test(lower) && lower.slice(3).startsWith(issue[1])
          : lower.includes(typed);
        if (!matches) continue;
        const introduced = this.tickets[id];
        hits.push({
          label: id,
          meta: `first in ${introduced.slice(0, 3).join(", ")}${introduced.length > 3 ? "\u2026" : ""}`,
          ticket: id,
        });
      }
      for (const a of this.data.notAffected) {
        if (hits.length >= 20) break;
        const haystack = `${a.id} ${a.component}`.toLowerCase();
        if (haystack.includes(q) && !hits.some((h) => h.cve === a.id || h.bundled === a.id)) {
          hits.push({ label: a.id, meta: `${a.component} · not affected`, bundled: a.id });
        }
      }
      this.hits = hits;
      this.activeHit = -1;
      results.innerHTML = String(
        html`${hits.map(
          (h, i) => html`
          <div class="otpv-hit" role="option" id="otpv-hit-${i}" aria-selected="false" data-i="${i}">
            <span class="otpv-hit-k">${h.label}</span>
            <span class="otpv-hit-m">${h.meta}</span>
          </div>`
        )}`
      );
      search.setAttribute("aria-expanded", hits.length ? "true" : "false");
      search.removeAttribute("aria-activedescendant");
    };

    const closeResults = () => {
      results.innerHTML = "";
      this.hits = [];
      this.activeHit = -1;
      search.setAttribute("aria-expanded", "false");
      search.removeAttribute("aria-activedescendant");
    };

    const moveActive = (delta: number) => {
      if (!this.hits.length) return;
      const previous = results.querySelector('[aria-selected="true"]');
      if (previous) previous.setAttribute("aria-selected", "false");
      this.activeHit = (this.activeHit + delta + this.hits.length) % this.hits.length;
      const option = results.querySelector(`#otpv-hit-${this.activeHit}`);
      if (option) {
        option.setAttribute("aria-selected", "true");
        option.scrollIntoView({ block: "nearest" });
        search.setAttribute("aria-activedescendant", option.id);
      }
    };

    search.addEventListener("input", runSearch);
    search.addEventListener("focus", runSearch);
    search.addEventListener("keydown", (e) => {
      if (e.key === "ArrowDown") { e.preventDefault(); moveActive(1); }
      else if (e.key === "ArrowUp") { e.preventDefault(); moveActive(-1); }
      else if (e.key === "Escape") { closeResults(); }
      else if (e.key === "Enter" && this.hits.length) {
        e.preventDefault();
        // With nothing highlighted, Enter takes the first result.
        activateHit(this.activeHit === -1 ? 0 : this.activeHit);
      }
    });

    const activateHit = (index: number) => {
      const hit = this.hits[index];
      if (!hit) return;
      closeResults();
      if (hit.version) {
        this.setHighlight(null);
        search.value = "";
        this.select(hit.version, { scroll: "smooth", push: true });
      } else if (hit.app) {
        this.setHighlight({ app: this.data.strs.indexOf(hit.app) });
        search.value = hit.app;
        const carrying = this.appIndex.get(hit.app)!;
        this.select([...carrying].sort((a, b) => this.position.get(a)! - this.position.get(b)!)[0], { scroll: "smooth", push: true });
      } else if (hit.cve) {
        search.value = hit.cve;
        this.setHighlight({ cve: hit.cve });
        this.expandedCves.add(hit.cve);
        const affected = this.affectedBy(hit.cve);
        if (affected.length) this.select(affected[affected.length - 1].v, { scroll: "smooth", push: true });
        else this.renderTree();
      } else if (hit.ticket) {
        search.value = hit.ticket;
        this.setHighlight({ ticket: hit.ticket });
        // Land on the release that introduced it; the rest follow by ordering.
        const introduced = this.tickets[hit.ticket];
        if (introduced?.length) this.select(introduced[0], { scroll: "smooth", push: true });
        else this.renderTree();
      } else {
        // A bundled-component statement is a dismissal, so no release is
        // affected and there is nothing to pick out in the tree.
        search.value = hit.bundled!;
        this.setHighlight(null);
        const majors = new Set(
          this.data.notAffected.filter((a) => a.id === hit.bundled).map((a) => a.major)
        );
        const newest = this.nodes.filter((n) => majors.has(n.major)).pop();
        if (newest) this.select(newest.v, { scroll: "smooth", push: true });
        else this.renderTree();
      }
    };

    results.addEventListener("click", (e) => {
      const option = (e.target as HTMLElement).closest<HTMLElement>(".otpv-hit");
      if (option) activateHit(Number(option.dataset.i));
    });

    document.addEventListener("click", (e) => {
      if (!(e.target as HTMLElement).closest(".otpv-search")) closeResults();
    });

    // Going back should return to the version you were looking at.
    window.addEventListener("popstate", () => {
      const v = this.versionFromUrl() ?? this.nodes[this.nodes.length - 1].v;
      this.select(v, { scroll: "smooth" });
    });
  }
}

async function fetchJson<T>(url: string): Promise<T> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${url}: ${response.status} ${response.statusText}`);
  return (await response.json()) as T;
}

async function init(): Promise<void> {
  const root = document.getElementById("otp-versions");
  if (!root) return;
  const url = root.dataset.src;
  if (!url) return;
  try {
    const [data, tickets] = await Promise.all([
      fetchJson<VersionData>(url),
      // Optional: the tree works without it, only the ticket search goes away.
      root.dataset.tickets ? fetchJson<Tickets>(root.dataset.tickets).catch(() => ({})) : {},
    ]);
    new VersionTree(data, tickets as Tickets, root);
  } catch (error) {
    const status = document.getElementById("otpv-status");
    if (status) {
      status.textContent =
        "The version data could not be loaded, so the tree is unavailable. Reloading the page may help.";
    }
    console.error("otp-versions:", error);
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", init);
} else {
  void init();
}
