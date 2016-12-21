#!/usr/bin/env python2

# print 'Usage: ' + sys.argv[0] + ' doc/ref/*.md'

from __future__ import print_function
import os
import re
import sys

if len(sys.argv) < 2:
    pass

def find_schema(txt):
    found = re.findall('#### Schema\n\n[^>]*\n\n\n', txt, re.MULTILINE | re.DOTALL)
    return found[0]

def public_doc(ref_path):
    doc_root = os.path.dirname(ref_path)
    ref_name = os.path.basename(ref_path)
    ref_to_doc = {
        'api_auth.md': 'api_authentication.md',
        'conferences.md': 'conference.md',
        'user_auth.md': 'user_authentication.md',
        'vmboxes.md': 'voicemail.md',
        'whitelabel.md': 'whitelabeling.md',
    }
    return os.path.join(doc_root, ref_to_doc.get(ref_name, ref_name))

errors = 0
for fname in sys.argv[1::]:
    docname = public_doc(fname)
    try:
        with open(fname, 'r') as f:
            schemas = find_schema(f.read())
    except IndexError:
        # print('No schemas found, ignoring', fname)
        continue

    if not os.path.isfile(docname):
        print('Doc does not exist, skipping', docname, file=sys.stderr)
        # errors += 1
        continue

    with open(docname, 'r') as f:
        whole_doc = f.read()
    outdated = find_schema(whole_doc)
    updated = whole_doc.replace(outdated, schemas)
    try:
        with open(docname, 'w') as f:
            f.write(updated)
    except IndexError:
        print('These missing schemas need to be manually added to', docname, file=sys.stderr)
        print(schemas, file=sys.stderr)
        errors += 1

sys.exit(errors)
