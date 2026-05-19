new Crawler({
  appId: "LUYTU1J2MB",
  apiKey: "secret",
  rateLimit: 8,
  sitemaps: ["https://www.erlang.org/docs/sitemap_algolia.xml"],
  cache: { enabled: false },
  renderJavaScript: false,
  ignoreCanonicalTo: true,
  discoveryPatterns: [
    "https://www.erlang.org/doc/**",
    "https://www.erlang.org/docs/**",
  ],
  schedule: "at 20:40 on Tuesday",
  actions: [
    {
      indexName: "erlang",
      pathsToMatch: [
        "https://www.erlang.org/doc/**",
        "https://www.erlang.org/docs/**",
      ],
      recordExtractor: ({ $, url }) => {
        try {
          const generator = $("meta[name=generator]").attr("content");
          const vsn = $("meta[name=major-vsn]").attr("content");
          const root = url.pathname.startsWith("/doc/")
            ? "/doc/"
            : "/docs/" + vsn + "/";
          console.log(url.pathname);
          if (
            url.pathname.match(
              /(index|users_guide|notes|release_notes|api-reference.html|404|search|nyi|man_index)(\.html)?$/,
            ) ||
            !url.pathname.match(/\.html$/)
          ) {
            return [];
          }
          if (generator && generator.startsWith("ExDoc")) {
            const collapseAndTrim = (/** @type {string} */ str) =>
              str.replace(/[\s]+/g, " ").trim();
            const assert = (
              /** @type {boolean} */ condition,
              /** @type {string} */ message,
            ) => {
              if (!condition) throw Error("Assert failed: " + (message || ""));
            };

            if ($('script:contains("window.location.replace")').length > 0) {
              return [];
            }

            // @ts-ignore
            const removeHtml = (elem) => {
              let html = elem
                .toString()
                .replace(/<span class="sr-only">View Source<\/span>/g, "")
                .replace(/<span class="sr-only">Copy Markdown<\/span>/g, "");
              // ExDoc wraps each code token in its own <span> for
              // syntax highlighting. The generic space-replacement
              // below would otherwise turn `lists:foreach(Fun, List)`
              // into `lists : foreach ( Fun, List )` in the indexed
              // content and snippets. Strip tags inside <code> and
              // <pre> without inserting whitespace first.
              html = html.replace(
                /<(code|pre)([^>]*)>([\s\S]*?)<\/\1>/g,
                (_, tag, attrs, inner) =>
                  `<${tag}${attrs}>${inner.replace(/<\/?[^>]+>/g, "")}</${tag}>`,
              );
              return $("<div />")
                .html(
                  html
                    .replace(/<\/?[^>]+>/g, " ")
                    .replace(/\s\s+/g, " ")
                    .replace(/ ([.,])/g, "$1")
                    .trim(),
                )
                .text();
            };

            console.log("Parsing as ExDoc: vsn=" + vsn);

            let recs = []; // The records to return
            let cnt = 0; // The number of records found do far
            const app = collapseAndTrim($(".sidebar-projectName").text());
            // Type can be "Application", "Command", "Module", "File",
            //    "User's Guide"
            let docType = $("body").attr("data-type");
            if (docType == undefined) docType = $("main").attr("data-type");
            if (docType == "extras") {
              if (url.href.endsWith("_app")) {
                docType = "Application";
              } else if (url.href.endsWith("_cmd")) {
                docType = "Command";
              } else if (
                $("a[title='View Source']")
                  .attr("href")
                  .match(/reference_manual/)
              ) {
                docType = "Erlang Reference Manual";
              } else if (
                $("a[title='View Source']")
                  .attr("href")
                  .match(/system\/doc/)
              ) {
                docType = "Erlang System Documentation";
              } else {
                docType = "User's Guide";
              }
            } else {
              assert(docType == "modules");
              docType = "Module";
            }
            const isUsersGuide = docType != "Module";
            // Module pages emit `<h1><span>name</span> <small>vsn</small></h1>`
            // so `h1 > span` is the right selector. User guides,
            // command pages and system docs emit a plain `<h1>Title</h1>`
            // with no inner span — fall through to the bare h1 text.
            const mod = isUsersGuide
              ? collapseAndTrim($("h1").first().text())
              : $("h1 > span").first().text();

            console.log("docType:" + docType);

            const createRecord = (props) => {
              let level = 0;

              props.content = collapseAndTrim(props.content).substring(
                0,
                90000,
              );

              // Skip section/anchor records that ended up with no
              // narrative text. They show up in results as
              // no-excerpt hits that just jump to a heading. The
              // module-overview record (anchor === "") is kept even
              // with empty content because the page itself is the
              // landing target.
              if (props.anchor !== "" && props.content === "") return null;

              console.log(props.content);

              // Add null to all hierarchy properties that are not set
              for (let i = 0; i < 7; i++) {
                if (props.hierarchy.hasOwnProperty("lvl" + i)) {
                  props.hierarchy["lvl" + i] = collapseAndTrim(
                    props.hierarchy["lvl" + i],
                  );
                  level++;
                } else {
                  props.hierarchy["lvl" + i] = null;
                }
                if (props.hierarchy_radio.hasOwnProperty("lvl" + i)) {
                  if (!Array.isArray(props.hierarchy_radio["lvl" + i])) {
                    props.hierarchy_radio["lvl" + i] = [
                      collapseAndTrim(props.hierarchy_radio["lvl" + i]),
                    ];
                  }
                } else {
                  props.hierarchy_radio["lvl" + i] = null;
                }
              }

              assert(
                props.anchor != undefined,
                "anchor is undefined" + JSON.stringify(props.hierarchy),
              );
              if (docType != "Module") {
                props.mfa = null;
              }

              if (props.namePrefixes === undefined) {
                props.namePrefixes = null;
              }
              if (props.modNamePrefixes === undefined) {
                props.modNamePrefixes = null;
              }

              // Derive what the record points at, from the URL
              // anchor: `t:foo/0` -> type, `c:foo/2` -> callback,
              // anything else inside a module page -> function.
              // null for module-overview records and for sections in
              // guides/commands/system docs.
              let kind = null;
              if (props.isModule && props.anchor) {
                if (props.anchor.startsWith("t:")) kind = "type";
                else if (props.anchor.startsWith("c:")) kind = "callback";
                else kind = "function";
              }

              const anchorSuffix = props.anchor ? "#" + props.anchor : "";

              return {
                objectID: ++cnt + "-" + url + "#" + props.anchor,
                version: vsn,
                tags: [...new Set([docType, app])], // Use Set to remove duplicates
                docEngine: "ex_doc",
                docType,
                app,
                kind,
                url: url + anchorSuffix,
                url_without_variables: url + anchorSuffix,
                url_without_anchor: url,
                lang: "en",
                type: "content", // Must be content
                no_variables: false,
                weight: {
                  pageRank:
                    app == "Erlang Reference Manual"
                      ? "2"
                      : docType == "User's Guide"
                        ? "1"
                        : "3",
                  level,
                  position: cnt,
                },
                isModule: props.isModule ? true : false, // This is used by the instantsearch frontend
                hierarchy_camel: props.hierarchy,
                hierarchy_radio_camel: props.hierarchy_radio,
                ...props,
              };
            };

            // push the module description
            let overviewContent = removeHtml(
              isUsersGuide
                ? $("h1").nextUntil("h1, h2, h3, h4, h5")
                : $("#moduledoc").children().first().nextUntil("h2"),
            );
            // Reference-manual chapter pages and similar user-guide
            // pages put their h1 immediately before an h2, so the
            // gap above contains only ExDoc's action links
            // (Copy Markdown / View Source) which `removeHtml`
            // strips to nothing. Fall back to the first paragraph
            // of the first h2 section so the overview record has a
            // useful excerpt instead of just the title.
            if (isUsersGuide && !overviewContent) {
              overviewContent = removeHtml(
                $("#top-content").find("h2").first().next("p"),
              );
            }
            recs.push(
              createRecord({
                anchor: "",
                content: overviewContent,
                hierarchy: {
                  lvl0: app,
                  lvl1: mod,
                },
                hierarchy_radio: {
                  // We include the docType to the lvl1 radio in order to be able
                  // to search for "ssl app", "config file" "erl command"
                  lvl1: mod + " " + docType,
                },
                isModule: docType == "Module",
                modNamePrefixes: docType == "Module" ? [mod] : [],
                mfa: docType == "Module" ? mod : null,
              }),
            );

            // push any functions
            const funcs = $("section.detail");
            let allAnchors = [];
            funcs.each((i, fun) => {
              // If an id is already used, we use the gh-id
              // This type of anchor exists for example in system_info-1
              let anchor = fun.attribs["id"];
              allAnchors.push(anchor);

              // name is the full name, i.e. map(Fun, List) -> List
              const name = $(".signature", fun).text().trim();
              // onlyName is the function name, i.e. map
              const onlyName = name.split("(")[0].trim();

              // https://www.algolia.com/doc/guides/managing-results/optimize-search-results/override-search-engine-defaults/how-to/how-can-i-make-queries-within-the-middle-of-a-word/
              let onlyNamePrefixes = [];
              for (let j = 0; j < onlyName.length; j++) {
                onlyNamePrefixes.push(onlyName.slice(j));
              }
              const modNamePrefixes = onlyNamePrefixes.map(
                (str) => mod + ":" + str,
              );

              let dochtml = $(".docstring", fun);
              $(".specs", dochtml).remove();
              $(".note", dochtml).remove();
              const docstring = removeHtml(dochtml);

              // the gl module has more than 1000 functions, so we any function that is an equiv
              if (mod == "gl" && docstring.match(/^Equivalent/)) {
                return;
              }

              recs.push(
                createRecord({
                  anchor,
                  content: docstring,
                  hierarchy: {
                    lvl0: app,
                    lvl1: mod,
                    lvl2: name,
                  },
                  hierarchy_radio: {
                    // For functions we add mod:name as the lvl1 radio hierachy
                    // so that we raise the ranking of functions
                    lvl1: mod + ":" + onlyName,
                    lvl2: name,
                  },
                  namePrefixes: onlyNamePrefixes,
                  modNamePrefixes,
                  mfa: mod + ":" + onlyName,
                  isModule: true,
                }),
              );
            });

            // Extract all h2 sections from a .md or moduledoc
            let sections = [];
            (isUsersGuide ? $("#top-content") : $("#moduledoc"))
              .children("h2")
              .each((index, element) => {
                const header = $(element);
                const nextElements = $(element).nextUntil("h2");
                sections.push({ title: header, content: nextElements });
              });

            const parseDefinitionList = (
              content,
              titleId,
              hierarchy,
              level,
            ) => {
              const dls = [];
              content.filter("ul").each((i, ul) => {
                const maybeP = $(ul).children().first().children().first();
                if (maybeP[0] && maybeP[0].name == "p") {
                  const maybeStrong = maybeP.children().first();
                  if (maybeStrong[0] && maybeStrong[0].name == "strong") {
                    dls.push($(ul));
                  }
                }
              });
              if (dls.length > 0) {
                dls.forEach((e) => {
                  const ul = e;
                  ul.children().each((i, li) => {
                    const anchor =
                      $(li).find("[id]").length == 0
                        ? titleId
                        : $(li).find("[id]").first().attr("id");
                    const dt = removeHtml(
                      $("p:first-child > strong:first-child", li),
                    );
                    // Drop the dt+separator from the indexed
                    // content so it isn't duplicated with
                    // hierarchy.lvl3. Clone first to leave the
                    // original DOM intact for downstream
                    // extractors.
                    const $body = $(li).clone();
                    $body.find("p:first-child > strong:first-child").remove();
                    const content = removeHtml($body)
                      .replace(/^[\s\-–—:]+/, "");
                    recs.push(
                      createRecord({
                        anchor: anchor,
                        content: content,
                        hierarchy: { [level]: dt, ...hierarchy },
                        hierarchy_radio: { [level]: dt },
                      }),
                    );
                  });
                });
                return dls[0].prevUntil("h2, h3");
              } else {
                return content;
              }
            };

            // Here we parse each section in app/com/file
            sections.forEach(({ title, content }, i) => {
              const titleText = removeHtml($("h2 > span", title));
              const titleId = title.attr("id");
              const mainContent = content.first().nextUntil("h2, h3").addBack();
              const mainContentBeforeList = parseDefinitionList(
                mainContent,
                titleId,
                { lvl0: app, lvl1: mod, lvl2: titleText },
                "lvl3",
              );

              recs.push(
                createRecord({
                  anchor: titleId,
                  content: removeHtml(mainContentBeforeList),
                  hierarchy: {
                    lvl0: app,
                    lvl1: mod,
                    lvl2: titleText,
                  },
                  hierarchy_radio: {
                    lvl2: titleText,
                  },
                }),
              );
              content.next("h3").each((i, subsection) => {
                const subContent = $(subsection).nextUntil("h3");
                const subtitleText = removeHtml(
                  $("h3 > span", $(subsection).first()),
                );
                const subtitleId = $(subsection).first().attr("id");
                const subContentBeforeList = parseDefinitionList(
                  subContent,
                  subtitleId,
                  { lvl0: app, lvl1: mod, lvl2: titleText, lvl3: subtitleText },
                  "lvl4",
                );

                recs.push(
                  createRecord({
                    anchor: subtitleId,
                    content: removeHtml(subContentBeforeList),
                    hierarchy: {
                      lvl0: app,
                      lvl1: mod,
                      lvl2: titleText,
                      lvl3: subtitleText,
                    },
                    hierarchy_radio: {
                      lvl3: subtitleText,
                    },
                  }),
                );
              });
            });
            // createRecord returns null for empty-content section
            // records; drop them here so callers don't have to.
            return recs.filter((r) => r !== null);
          } else {
            /* Crawl erl_docgen starts here */
            const collapseAndTrim = (str) => str.replace(/[\s]+/g, " ").trim();
            const assert = (condition, message) => {
              if (!condition) throw Error("Assert failed: " + (message || ""));
            };

            let recs = []; // The records to return
            let cnt = 0; // The number of records found do far
            const app = collapseAndTrim($(".section-title").text());
            // Type can be "Application", "Command", "Module", "File",
            //    "User's Guide"
            const docType =
              $(".section-subtitle").text() == "User's Guide"
                ? "User's Guide"
                : $(".section-subtitle").text() == "Internal Documentation"
                  ? "Internal Documentation"
                  : $("#content h3 .title-name").html();
            const isUsersGuide =
              docType == "User's Guide" || docType == "Internal Documentation";
            const mod = isUsersGuide
              ? $(".innertube h1")
                  .text()
                  .replace(/^[0-9.]+/, "")
              : $(".innertube h1").text();

            const createRecord = (props) => {
              let level = 0;

              props.content = collapseAndTrim(props.content);

              // Add null to all hierarchy properties that are not set
              for (let i = 0; i < 7; i++) {
                if (props.hierarchy.hasOwnProperty("lvl" + i)) {
                  props.hierarchy["lvl" + i] = collapseAndTrim(
                    props.hierarchy["lvl" + i],
                  );
                  level++;
                } else {
                  props.hierarchy["lvl" + i] = null;
                }
                if (props.hierarchy_radio.hasOwnProperty("lvl" + i)) {
                  if (!Array.isArray(props.hierarchy_radio["lvl" + i])) {
                    props.hierarchy_radio["lvl" + i] = [
                      collapseAndTrim(props.hierarchy_radio["lvl" + i]),
                    ];
                  }
                } else {
                  props.hierarchy_radio["lvl" + i] = null;
                }
              }

              assert(
                props.anchor != undefined,
                "anchor is undefined" + JSON.stringify(props.hierarchy),
              );
              if (docType != "Module") {
                props.mfa = null;
              }

              if (props.namePrefixes === undefined) {
                props.namePrefixes = null;
              }
              if (props.modNamePrefixes === undefined) {
                props.modNamePrefixes = null;
              }

              return {
                objectID: ++cnt + "-" + url + "#" + props.anchor,
                version: vsn,
                tags: [...new Set([docType, app])], // Use Set to remove duplicates
                docEngine: "erl_docgen",
                docType,
                app,
                url: url + "#" + props.anchor,
                url_without_variables: url + "#" + props.anchor,
                url_without_anchor: url,
                lang: "en",
                type: "content", // Must be content
                no_variables: false,
                weight: {
                  pageRank:
                    app == "Erlang Reference Manual"
                      ? "2"
                      : docType == "User's Guide"
                        ? "1"
                        : "3",
                  level,
                  position: cnt,
                },
                isModule: props.isModule ? true : false, // This is used by the instantsearch frontend
                hierarchy_camel: props.hierarchy,
                hierarchy_radio_camel: props.hierarchy_radio,
                ...props,
              };
            };

            // push the module description
            recs.push(
              createRecord({
                anchor: "",
                content: isUsersGuide
                  ? $("#content h1").nextUntil("h1, h2, h3, h4, h5").text()
                  : $(".description-body").text(),
                hierarchy: {
                  lvl0: app,
                  lvl1: mod,
                },
                hierarchy_radio: {
                  // We include the docType to the lvl1 radio in order to be able
                  // to search for "ssl app", "config file" "erl command"
                  lvl1: mod + " " + docType,
                },
                isModule: docType == "Module",
                modNamePrefixes: docType == "Module" ? [mod] : [],
                mfa: docType == "Module" ? mod : null,
              }),
            );

            // push any functions
            const funcs = $(".func h4");
            let allAnchors = [];
            funcs.each((i, fun) => {
              // If an id is already used, we use the gh-id
              // This type of anchor exists for example in system_info-1
              let anchor = fun.attribs["id"];
              if (allAnchors.findIndex((i) => i == anchor) != -1) {
                anchor = $(".ghlink-before", fun).attr("id");
              }
              allAnchors.push(anchor);

              // name is the full name, i.e. map(Fun, List) -> List
              const name = $(".title-name", fun).text().trim();
              // onlyName is the function name, i.e. map
              const onlyName = name.split("(")[0].trim();

              // the gl module has more than 1000 functions, so we remove functions
              // without documentation or if they end in ARB.
              if (
                mod == "gl" &&
                ($(fun).next().is("h4") || onlyName.endsWith("ARB")) &&
                !onlyName.match(/^'?begin/)
              ) {
                return;
              }

              // https://www.algolia.com/doc/guides/managing-results/optimize-search-results/override-search-engine-defaults/how-to/how-can-i-make-queries-within-the-middle-of-a-word/
              let onlyNamePrefixes = [];
              for (i = 0; i < onlyName.length; i++) {
                onlyNamePrefixes.push(onlyName.slice(i));
              }
              const modNamePrefixes = onlyNamePrefixes.map(
                (str) => mod + ":" + str,
              );

              recs.push(
                createRecord({
                  anchor,
                  content: $(fun)
                    .closest("article")
                    .find(".exports-tube")
                    .text(),
                  hierarchy: {
                    lvl0: app,
                    lvl1: mod,
                    lvl2: name,
                  },
                  hierarchy_radio: {
                    // For functions we add mod:name as the lvl1 radio hierachy
                    // so that we raise the ranking of functions
                    lvl1: mod + ":" + onlyName,
                    lvl2: name,
                  },
                  namePrefixes: onlyNamePrefixes,
                  modNamePrefixes,
                  mfa: mod + ":" + onlyName,
                  isModule: true,
                }),
              );
            });

            // Here we parse each section in app/com/file
            const sections = $("section.innertube");
            sections.each((i, section) => {
              const dl = $(".REFBODY dl", section);

              // if we don't find any definition list
              // we just add the entire section to the search
              if (dl.length == 0) {
                recs.push(
                  createRecord({
                    anchor: $("h3", section).attr("id"),
                    content: $(".REFBODY", section).text(),
                    hierarchy: {
                      lvl0: app,
                      lvl1: mod,
                      lvl2: $(".title-name", section).text(),
                    },
                    hierarchy_radio: {
                      lvl2: $(".title-name", section).text(),
                    },
                  }),
                );
              } else {
                // If we do find a definition list
                // we add each item as a record to the search
                $("dt", dl).each((i, dt) => {
                  const anchor =
                    $(dt).find("[name]").length == 0
                      ? $("h3", section).attr("id")
                      : $(dt).find("[name]").first().attr("name");
                  recs.push(
                    createRecord({
                      anchor: anchor,
                      content: $(dt).next("dd").text(),
                      hierarchy: {
                        lvl0: app,
                        lvl1: mod,
                        lvl2: $(".title-name", section).text(),
                        lvl3: $(dt).text(),
                      },
                      hierarchy_radio: {
                        lvl3: $(dt).text(),
                      },
                    }),
                  );
                });
              }
            });

            // Here we parse sections in User's Guides
            $(".innertube h3").each((i, section) => {
              recs.push(
                createRecord({
                  anchor: $(section).attr("id"),
                  content: $(section).nextUntil("h3, h4").text(),
                  hierarchy: {
                    lvl0: app,
                    lvl1: mod,
                    lvl2: $(".title-name", section)
                      .text()
                      .replace(/^[0-9.]*/, ""),
                  },
                  hierarchy_radio: {
                    lvl2: $(".title-name", section)
                      .text()
                      .replace(/^[0-9.]*/, ""),
                  },
                }),
              );
              let subSection = $(section).nextUntil("h3, h4").last().next();
              while (subSection.is("h4")) {
                recs.push(
                  createRecord({
                    anchor: $(subSection).attr("id"),
                    content: $(subSection).nextUntil("h3, h4").text(),
                    hierarchy: {
                      lvl0: app,
                      lvl1: mod,
                      lvl2: $(".title-name", section)
                        .text()
                        .replace(/^[0-9.]*/, ""),
                      lvl3: $(".title-name", subSection).text(),
                    },
                    hierarchy_radio: {
                      lvl3: $(".title-name", subSection).text(),
                    },
                  }),
                );
                subSection = $(subSection).nextUntil("h3, h4").last().next();
              }
            });

            return recs;
          }
        } catch (e) {
          console.log(e.stack);
          return [];
        }
      },
    },
  ],
  initialIndexSettings: {
    erlang: {
      // `kind` ('function' | 'type' | 'callback') is derived per
      // record from the URL anchor — facetable so /doc/search.html
      // can offer it as a filter. `type` is dead-weight (every
      // record hardcodes "content").
      attributesForFaceting: ["kind", "lang"],
      attributesToRetrieve: ["hierarchy", "content", "anchor", "url"],
      attributesToHighlight: ["hierarchy", "hierarchy_camel", "content"],
      attributesToSnippet: ["content:10"],
      camelCaseAttributes: ["hierarchy", "hierarchy_radio", "content"],
      searchableAttributes: [
        "unordered(hierarchy_radio_camel.lvl1)",
        "unordered(hierarchy_radio.lvl1)",
        "hierarchy_radio_camel.lvl2",
        "hierarchy_radio.lvl2",
        "unordered(hierarchy_radio_camel.lvl0)",
        "unordered(hierarchy_radio.lvl0)",
        "unordered(hierarchy_radio_camel.lvl3)",
        "unordered(hierarchy_radio.lvl3)",
        "unordered(hierarchy_camel.lvl0)",
        "unordered(hierarchy.lvl0)",
        "unordered(hierarchy_camel.lvl1)",
        "unordered(hierarchy.lvl1)",
        "unordered(hierarchy_camel.lvl2)",
        "unordered(hierarchy.lvl2)",
        "unordered(hierarchy_camel.lvl3)",
        "unordered(hierarchy.lvl3)",
        "content",
      ],
      distinct: true,
      attributeForDistinct: "url",
      customRanking: [
        "desc(weight.pageRank)",
        "desc(weight.level)",
        "asc(weight.position)",
      ],
      ranking: [
        "words",
        "filters",
        "typo",
        "attribute",
        "proximity",
        "exact",
        "custom",
      ],
      highlightPreTag: '<span class="algolia-docsearch-suggestion--highlight">',
      highlightPostTag: "</span>",
      minWordSizefor1Typo: 3,
      minWordSizefor2Typos: 7,
      allowTyposOnNumericTokens: false,
      minProximity: 1,
      ignorePlurals: true,
      queryLanguages: ["en"],
      advancedSyntax: true,
      attributeCriteriaComputedByMinProximity: true,
      removeWordsIfNoResults: "allOptional",
      separatorsToIndex: "",
    },
  },
});
