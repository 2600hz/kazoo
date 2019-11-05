#!/usr/bin/env python3

import os
import re
import sys

if len(sys.argv) < 2:
    print('Usage: ' + sys.argv[0] + ' applications/crossbar/doc/ref/*.md', file=sys.stderr)
    sys.exit(1)

def find_schema(txt):
    found = re.findall('^#{2,} Schema\n\n[^>]*?(?=\n{3}\#{1,4}|\Z)', txt, re.MULTILINE | re.DOTALL)
    return found[0]

def public_doc(ref_path):
    ref_name = os.path.basename(ref_path)

    if ref_name == "skel.md":
        return "skip"

    if os.path.basename(os.path.dirname(ref_path)) == "ref":
        doc_root = os.path.dirname(os.path.dirname(ref_path))
    else:
        return "skip"

    ref_name = os.path.basename(ref_path)

    ref_to_doc = {
        'api_auth.md': 'api_authentication.md',
        'conferences.md': 'conference.md',
        'user_auth.md': 'user_authentication.md',
        'vmboxes.md': 'voicemail.md',
        'whitelabel.md': 'whitelabeling.md'
    }

    return os.path.join(doc_root, ref_to_doc.get(ref_name, ref_name))

errors = 0
for refname in sys.argv[1::]:
    docname = public_doc(refname)
    if docname == "skip":
        continue

    if not os.path.isfile(docname):
        print('Doc does not exist, please create', docname, '(from', refname, ')', file=sys.stderr)
        errors += 1
        continue

    try:
        with open(refname, 'r') as f:
            schemas = find_schema(f.read())
    except IndexError:
        # print('No schemas found, ignoring', refname)
        continue

    with open(docname, 'r') as f:
        whole_doc = f.read()
    try:
        outdated = find_schema(whole_doc)
    except IndexError:
        continue

    updated = whole_doc.replace(outdated, schemas)
    try:
        with open(docname, 'w') as f:
            f.write(updated)
    except IndexError:
        print('These missing schemas need to be manually added to', docname, file=sys.stderr)
        print(schemas, file=sys.stderr)
        errors += 1

sys.exit(errors)
