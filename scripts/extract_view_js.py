#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json
import jsbeautifier

def print_usage():
    banner = '''
╻┏ ┏━┓╺━┓┏━┓┏━┓   ╻ ╻╻┏━╸╻ ╻
┣┻┓┣━┫┏━┛┃ ┃┃ ┃   ┃┏┛┃┣╸ ┃╻┃
╹ ╹╹ ╹┗━╸┗━┛┗━┛   ┗┛ ╹┗━╸┗┻┛(o_O)
'''
    print(banner)
    print('Extract JavaScript inside a view function and save it to a file.')
    print('You know when you need this!\n')
    print('Usage: ' + sys.argv[0] + ' view_js design_doc view_name [view_function]\n')
    print('Mandatory options:')
    print('   view_js:        path to view JavaScript file')
    print('   design_doc:     path to view design document')
    print('   view_name:      the name of view in design doc')
    print('\nOptional options:')
    print('   view_function:  view function name to extract its JavaScript, i.e. map or reduce; Default: map')
    exit(1)

if len(sys.argv) < 4:
    print_usage()

jsfile = sys.argv[1]
design_file = sys.argv[2]
view_name = sys.argv[3]

if len(sys.argv) < 5:
    view_function = 'map'
elif sys.argv[4] in ['map', 'reduce']:
    view_function = sys.argv[4]
else:
    print_usage()

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

def read_js_file():
    try:
        with open(jsfile) as fd:
            return fd.read()
    except Exception as e:
        print('failed to read {}'.format(jsfile))
        print(e)
        exit(1)

def read_design():
    try:
        with open(design_file) as fd:
            return json.load(fd)
    except Exception as e:
        print('failed to read {}'.format(design_file))
        print(e)
        exit(2)

js = read_js_file()
design_doc = read_design()

try:
    design_doc['views'][view_name][view_function] = multiline_view(js)
    data = json.dumps(design_doc, sort_keys=True, indent=4, separators=(",", ": "))
    wrote = open(design_file, 'w').write(data + '\n')
    print('Wrote {}'.format(wrote))
except Exception as e:
    print('failed to repalce javascript for {}/{}'.format(view_name, view_function))
    print(e)
    exit(3)
