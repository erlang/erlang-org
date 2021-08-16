/*
 * This small script removed any empty <p></p> so that algolia indexer will work better.
 */
const HTMLParser = require('node-html-parser');
const fs = require('fs')
var data;

for (var i = 2; i < process.argv.length; i++) {
    var filename = process.argv[i];

    try {
        data = fs.readFileSync(filename, 'utf8');
    } catch (err) {
        console.log("read: " + filename + err);
        continue;
    }

    var root = HTMLParser.parse(data,{comment: true});
    const ps = root.querySelectorAll('p');
    for (const a in ps) {
        const p = ps[a];
        if (p.childNodes.length == 0) {
            p.remove();
        } else if (p.childNodes.length == 1) {
            if (p.childNodes[0].nodeType == 3) {
                if (p.childNodes[0].text.trim() == "") {
                    p.remove();
                }
            }
        }
    }

    try {
        fs.writeFileSync(filename, root.toString());
    } catch (err) {
        console.error("write: " + filename + err)
        continue;
    }
}
