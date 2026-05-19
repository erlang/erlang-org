#!/usr/bin/env node
//
// Add synthetic anchors to definition-list items that don't have
// one. ExDoc renders markdown definition lists as:
//
//   <ul>
//     <li>
//       <p><strong><code class="inline">term</code></strong> - description</p>
//     </li>
//     ...
//   </ul>
//
// Some OTP source pages add explicit anchors via markdown attribute
// lists (kramdown `{: #id }`), ending up as <code id="..."> in HTML.
// Others don't. Without an id, the crawler's parseDefinitionList
// falls back to the parent section's titleId, so every dl-item in
// the section collapses to the same URL. Deep links from search
// land on the section header rather than the specific item.
//
// This script walks every HTML page under <docs-dir>, finds the
// ExDoc dl pattern, and for each item without an existing id
// injects one onto the <code> (or onto the <strong> when no
// <code> is present) using a slug of the dt text. Collisions
// within a page get a numeric suffix. Idempotent: items that
// already carry an id are skipped.
//
// Regex-based instead of cheerio-based so the rewrite is purely
// additive — the rest of the file stays byte-identical, which
// keeps git diffs reviewable. ExDoc's HTML for dl items is
// uniform enough that the regex approach is reliable.
//
// The crawler picks the injected id up naturally via
// $(li).find("[id]") with no change on its side.
//
// Usage: node _scripts/add_dl_anchors.js <docs-dir>

const fs = require("fs");
const path = require("path");

function slugify(s) {
  return s
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[̀-ͯ]/g, "")
    // Keep +, _, - so anchors like +pad, c_node, M_cp survive
    // intact, matching ExDoc's own slug convention.
    .replace(/[^a-z0-9+_-]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function processFile(file) {
  const original = fs.readFileSync(file, "utf8");

  // Pre-seed the id collision set with every existing id in the
  // file (headings, links, ExDoc-added dl ids, etc.) so we never
  // shadow something that's already in use.
  const used = new Set();
  for (const m of original.matchAll(/\bid="([^"]+)"/g)) used.add(m[1]);

  const alloc = (base) => {
    let id = base;
    let n = 2;
    while (used.has(id)) id = `${base}-${n++}`;
    used.add(id);
    return id;
  };

  let changes = 0;

  // Pattern 1: dl-item with inner <code>, no id yet. The negative
  // lookahead on attrs catches `<code class="inline">`, but not
  // `<code class="inline" id="...">`.
  let out = original.replace(
    /<li>(\s*)<p>(\s*)<strong>(\s*)<code((?:\s+(?!id=)[a-z-]+="[^"]*")*)>([^<]+)<\/code>/g,
    (full, ws1, ws2, ws3, attrs, term) => {
      const slug = slugify(term.trim());
      if (!slug) return full;
      const id = alloc(slug);
      changes++;
      return `<li>${ws1}<p>${ws2}<strong>${ws3}<code${attrs} id="${id}">${term}</code>`;
    },
  );

  // Pattern 2: dl-item with plain <strong> (no inner <code>).
  // Pattern 1 already handled the inner-code case, so this only
  // fires on remaining bare-strong dl-items.
  out = out.replace(
    /<li>(\s*)<p>(\s*)<strong>([^<]+)<\/strong>/g,
    (full, ws1, ws2, term) => {
      const slug = slugify(term.trim());
      if (!slug) return full;
      const id = alloc(slug);
      changes++;
      return `<li>${ws1}<p>${ws2}<strong id="${id}">${term}</strong>`;
    },
  );

  if (changes > 0) fs.writeFileSync(file, out);
  return changes;
}

function walk(dir, out = []) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) walk(p, out);
    else if (e.isFile() && p.endsWith(".html")) out.push(p);
  }
  return out;
}

const docsDir = process.argv[2];
if (!docsDir) {
  console.error("usage: node add_dl_anchors.js <docs-dir>");
  process.exit(1);
}

const files = walk(docsDir);
let totalChanges = 0;
let filesChanged = 0;
for (const f of files) {
  const n = processFile(f);
  if (n > 0) {
    filesChanged++;
    totalChanges += n;
  }
}
console.log(
  `Added ${totalChanges} dl anchors across ${filesChanged}/${files.length} files`,
);
