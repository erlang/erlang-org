#!/bin/bash

ROOT=${1}
BASEURL=${2}
HTML_PAGES=$(find "${ROOT}" -name "*.html")
DATE=$(date -Iseconds)

cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>

<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
EOF

for page in ${HTML_PAGES}; do
    echo "   <url>
      <loc>${BASEURL}${page}</loc>
      <lastmod>${DATE}</lastmod>
      <changefreq>weekly</changefreq>
   </url>"
done

cat <<EOF
</urlset>
EOF
