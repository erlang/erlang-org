/**
 * The version scheme, which everything else on the version tree page rests on:
 * how releases are coloured against one another, whether an advisory still
 * applies, and which releases carry a ticket. The examples are the ones in the
 * System Principles guide, under "Version Scheme".
 *
 *   node --experimental-strip-types --test assets/otp-version-scheme.test.ts
 */
import test from "node:test";
import assert from "node:assert/strict";
import { compare, branchOf, baseOf, inAnyRange } from "./otp-version-scheme.ts";

/** How the guide writes a comparison, so the cases read like the document. */
const relation = (a: string, b: string): string => {
  const c = compare(a, b);
  return c === null ? "undefined" : c === 0 ? "=" : c < 0 ? "<" : ">";
};

test("a version is ordered against its ancestors and descendants only", () => {
  // Every case in the guide's diagram, read against 35.3.0.2.2.
  const cases: Array<[string, string]> = [
    ["35.2.7", "<"],
    ["35.3", "<"],
    ["35.3.1", "undefined"],
    ["35.3.0.1", "<"],
    ["35.3.0.2", "<"],
    ["35.3.0.3", "undefined"],
    ["35.3.0.4", "undefined"],
    ["35.3.0.2.1", "<"],
    ["35.3.0.2.2", "="],
    ["35.3.0.2.3", ">"],
    ["35.3.0.2.4", ">"],
    ["35.3.0.2.3.1", ">"],
  ];
  for (const [version, expected] of cases) {
    assert.equal(relation(version, "35.3.0.2.2"), expected, `${version} against 35.3.0.2.2`);
  }
});

test("ordering is antisymmetric, and undefined both ways round", () => {
  assert.equal(relation("28.0", "28.5.0.4"), "<");
  assert.equal(relation("28.5.0.4", "28.0"), ">");
  // Two maintenance lines: neither contains the other.
  assert.equal(relation("27.3.4.16", "28.5.0.5"), "undefined");
  assert.equal(relation("28.5.0.5", "27.3.4.16"), "undefined");
});

test("a version number says which branch it is on", () => {
  assert.equal(branchOf("29.0.5"), "trunk", "three components or fewer is the main track");
  assert.equal(branchOf("28.5"), "trunk");
  assert.equal(branchOf("27.3.4.16"), "27.3.4.");
  assert.equal(branchOf("18.3.4.1.1"), "18.3.4.1.", "a branch off a branch");
});

test("a branch identifier says which version it grew out of", () => {
  assert.equal(baseOf("18.2.4."), "18.2.4");
  // A second branch from the same base gets a 0 before the sequence number,
  // which is dropped again to recover the base.
  assert.equal(baseOf("18.2.4.0."), "18.2.4");
  // Branching from a version whose third component was omitted puts it back.
  assert.equal(baseOf("28.5.0."), "28.5");
  assert.equal(baseOf("20.2.0."), "20.2");
  assert.equal(baseOf("18.3.4.1."), "18.3.4.1", "the base is itself on a branch");
});

test("an advisory's bounded ranges cover from one version up to the fix", () => {
  // The shape a CVE record uses when it lists each maintenance line separately.
  const ranges = [
    { from: "18.1.4", until: "27.3.4.17" },
    { from: "28.0", until: "28.5.0.6" },
    { from: "29.0", until: "29.0.6" },
  ];
  assert.equal(inAnyRange("27.3.4.16", ranges), true);
  assert.equal(inAnyRange("27.3.4.17", ranges), false, "the fix itself is not affected");
  assert.equal(inAnyRange("29.0.5", ranges), true);
  assert.equal(inAnyRange("29.0.6", ranges), false);
  assert.equal(inAnyRange("18.1.3", ranges), false, "before the flaw was introduced");
});

test("an advisory's open range is closed by a fix on each line", () => {
  // The commoner shape: affected from where it was introduced, with a fix
  // recorded per maintenance line.
  const ranges = [{ from: "17.0", fixedAt: ["28.0.1", "27.3.4.1", "26.2.5.13"] }];
  assert.equal(inAnyRange("20.3.8.26", ranges), true, "an old release no fix reaches");
  assert.equal(inAnyRange("26.2.5.12", ranges), true);
  assert.equal(inAnyRange("26.2.5.13", ranges), false, "fixed on its own line");
  assert.equal(inAnyRange("26.2.5.20", ranges), false, "and everything after it");
  assert.equal(inAnyRange("28.0.1", ranges), false);
  assert.equal(inAnyRange("28.5", ranges), false, "a descendant of a fix");
  assert.equal(inAnyRange("16.9", ranges), false, "before the flaw was introduced");
});

test("a fix on one line does not clear another", () => {
  // 27.3.4.1 fixes the 27.3.4. branch and nothing else: 28.0 has no order
  // against it, so the fix that matters there is 28.0.1.
  const ranges = [{ from: "17.0", fixedAt: ["27.3.4.1"] }];
  assert.equal(inAnyRange("27.3.4.2", ranges), false);
  assert.equal(inAnyRange("28.0", ranges), true, "a different line, still affected");
});
