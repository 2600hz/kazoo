#!/usr/bin/env node

const fs = require('fs');
const lunr = require('lunr');

const docsFile = process.argv[2];
const indexFile = process.argv[3];

if (!docsFile) {
    console.log("no source index document was given");
    usage();
}
if (!indexFile) {
    console.log("no path to where to save index was given");
    usage();
}

function usage() {
    console.log(__filename + " doc/edoc/tmp/search-docs.json doc/edoc/js/search_index.js");
    process.exit(1);
}

let documents;

try {
    documents = JSON.parse(fs.readFileSync(docsFile))
} catch (e) {
    console.error('There was an error reading the file!', e);
    process.exit(1);
}


lunr.tokenizer.separator = /[^a-zA-Z0-9]/;

var idx = lunr(function() {
    this.ref('ref');
    this.field('app');
    this.field('module');
    this.field('fun');
    this.field('type');
    this.field('desc');

    documents.forEach(function(doc) {
        this.add(doc)
    }, this)
})

const jsonIdx = JSON.stringify(idx);

try {
    fs.writeFileSync(indexFile, 'window.searchIdx = ' + jsonIdx);
} catch (e) {
    console.error('There was an error writing the index file!', e);
    process.exit(1);
}

console.log('created!');
