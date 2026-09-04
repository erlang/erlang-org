/**
 * The Erlang/OTP version scheme, as described in the System Principles guide
 * under "Version Scheme".
 *
 * A version number carries the whole structure: which branch it sits on, what
 * that branch grew out of, and how it orders against any other version. Nothing
 * here needs the page, or a browser, or the generated data — which is the point,
 * since everything the page says about releases follows from these rules.
 */

const components = (v: string): number[] => v.split(".").map(Number);

/**
 * The branch identifier is every component but the last, keeping trailing
 * zeroes. Three components or fewer means the trunk.
 */
export function branchOf(v: string): string {
  const p = components(v);
  return p.length <= 3 ? "trunk" : p.slice(0, -1).join(".") + ".";
}

/**
 * The version a branch grew out of: its identifier with trailing zeroes
 * dropped, but never below two components.
 */
export function baseOf(branch: string): string {
  const p = branch.slice(0, -1).split(".").map(Number);
  while (p.length > 2 && p[p.length - 1] === 0) p.pop();
  return p.join(".");
}

/**
 * Compares two versions from the same version tree.
 *
 * Returns a negative number, zero or a positive number in the usual way, or
 * `null` when the two are not ordered at all — neither is an ancestor of the
 * other, so nothing follows about what either one contains. This mirrors
 * `versions:compare/2` in the `runtime_tools` application.
 */
export function compare(a: string, b: string): number | null {
  const A = components(a);
  const B = components(b);
  let i = 0;
  while (i < A.length && i < B.length && A[i] === B[i]) i++;
  if (i === A.length && i === B.length) return 0;
  if (i === A.length) return -1;
  if (i === B.length) return 1;

  const aIsSmaller = A[i] < B[i];
  const smaller = aIsSmaller ? A : B;
  // The smaller side settles it when it is a normal version, or when the
  // differing component is its last one. Otherwise there is no order.
  if (smaller.length <= 3 || i === smaller.length - 1) return aIsSmaller ? -1 : 1;
  return null;
}

/**
 * Whether a release falls in any affected range of a CVE record. A range is
 * either bounded — affected from `from` until `until` — or open from `from`
 * with a fix point per maintenance line, in which case a release is safe once
 * it is one of those points or a descendant of one.
 */
export function inAnyRange(
  version: string,
  ranges: Array<{ from: string; until?: string; fixedAt?: string[] }>
): boolean {
  const atOrAfter = (a: string, b: string) => {
    const c = compare(a, b);
    return c !== null && c >= 0;
  };
  return ranges.some((range) => {
    if (!atOrAfter(version, range.from)) return false;
    if (range.until) {
      const c = compare(version, range.until);
      return c !== null && c < 0;
    }
    return !(range.fixedAt ?? []).some((fix) => atOrAfter(version, fix));
  });
}
