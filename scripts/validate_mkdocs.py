#!/usr/bin/env python3

import yaml, os.path, sys
from functools import reduce

# from https://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def parse_page_dict(errors_detected, kv):
    (header, pages) = kv
    if pages is None:
        eprint("section ", header, " is incomplete")
        return True
    else:
        return parse_page(errors_detected, pages)

def parse_page_string(errors_detected, page):
    if "index.md" != page and (not os.path.isfile(page)):
        eprint("page ", page, " is not valid")
        return True
    else:
        return errors_detected

def parse_page(errors_detected, page):
    "parse a page for existence"
    if isinstance(page, dict):
        return reduce(parse_page_dict, list(page.items()), errors_detected)
    elif isinstance(page, list):
        return reduce(parse_page, page, errors_detected)
    elif isinstance(page, str):
        return parse_page_string(errors_detected, page)
    else:
        return errors_detected

stream = open("doc/mkdocs/mkdocs.yml", 'r')
mkdocs = yaml.safe_load_all(stream)
errors_detected = False

for doc in mkdocs:
    for k,v in list(doc.items()):
        if "pages" == k:
            errors_detected = parse_page(False, v)

if errors_detected:
    sys.exit(1)
