#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json
from subprocess import call
import os
import shutil
import re
import jsbeautifier

if len(sys.argv) < 2:
    print('Usage: ' + sys.argv[0] + ' file.json+')
    exit(0)

design_pattern = re.compile('^_design/')

def multiline_view(js):
    opts = jsbeautifier.default_options()
    opts.indent_size = 2

    js = jsbeautifier.beautify(''.join(js), opts)
    multiLine = []
    for line in js.split('\n'):
        if not line:
            continue
        multiLine.append(line)
    return multiLine

def dict_map(data):
    for key, value in data.items():
        if isinstance(value, str):
            if value.startswith('function'):
                data[key] = multiline_view(value)
        elif isinstance(value, list) and len(value) > 0 \
            and isinstance(value[0], str) and value[0].startswith('function'):
            data[key] = multiline_view(value)
        else:
            data[key] = value
    return data

def process_couchdb_view(data):
    for key, value in data.items():
        if key in ['filters', 'lists', 'shows', 'updates']:
            data[key] = dict_map(value)
        elif key == 'views':
            views = {}
            for view, view_funs in value.items():
                views[view] = dict_map(view_funs)
            data[key] = views
        elif key == "rewrites" and isinstance(value, str):
            data[key] = multiline_view(value)
        elif key == "validate_doc_update":
            data[key] = multiline_view(value)
        else:
            data[key] = value
    return data

exit_code = 0

def main():
    json.encoder.FLOAT_REPR = str
    for fn in sys.argv[1:]:
        if not os.path.isfile(fn):
            print('not a file: {}'.format(fn));
            continue
        if not fn.endswith('.json'):
            print('not a JSON file: {}'.format(fn));
            continue
        fn2 = fn + '~'
        with open(fn) as rd:
            data = json.load(rd)
            if not design_pattern.match(data['_id']):
                print('not a CouchDB view file: {}'.format(fn))
                continue
            try:
                data2 = json.dumps(process_couchdb_view(data), sort_keys=True, indent=4, separators=(",", ": "))
                with open(fn2, 'w') as fd2:
                    fd2.write(data2 + '\n')
                shutil.move(fn2, fn)
            except Exception as e:
                print('failed to process {}'.format(fn))
                print(e)
                global exit_code
                exit_code=1

main()
exit(exit_code)
