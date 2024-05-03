#!/bin/bash

VSNs="${1}"
LATEST="${1}"
BASEURL=${3}
DATE=$(date -Iseconds)

cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>

<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
EOF

HTML_PAGES=$(find "doc/" -name "*.html")
for page in ${HTML_PAGES}; do
            echo "   <url>
      <loc>${BASEURL}${page}</loc>
      <lastmod>${DATE}</lastmod>
      <changefreq>weekly</changefreq>
   </url>"
done

for VSN in ${VSNs}; do
    if [ "${VSN}" -gt 23 ] && [ "${VSN}" != "${LATEST}" ]; then
        HTML_PAGES=$(find "docs/${VSN}" -name "*.html")
        for page in ${HTML_PAGES}; do
            echo "   <url>
      <loc>${BASEURL}${page}</loc>
      <lastmod>${DATE}</lastmod>
      <changefreq>weekly</changefreq>
   </url>"
        done
    fi
done

cat <<EOF
</urlset>
EOF
