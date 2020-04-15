#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
from subprocess import call
import json
import jsbeautifier
import shutil

def print_usage():
    banner = '''
╻┏ ┏━┓╺━┓┏━┓┏━┓   ╻ ╻╻┏━╸╻ ╻
┣┻┓┣━┫┏━┛┃ ┃┃ ┃   ┃┏┛┃┣╸ ┃╻┃
╹ ╹╹ ╹┗━╸┗━┛┗━┛   ┗┛ ╹┗━╸┗┻┛(o_O)
'''
    print(banner)
    print('Replace JavaScript inside a view function from a file.')
    print('You know when you need this!\n')
    print('Usage: ' + sys.argv[0] + ' view_js design_doc view_name [view_function]\n')
    print('Mandatory options:')
    print('   view_js:        path to view JavaScript file')
    print('   design_doc:     path to view design document')
    print('   view_name:      the name of view in design doc')
    print('\nOptional options:')
    print('   view_function:  view function name to replace its JavaScript, i.e. map or reduce; Default: map')
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

    js = jsbeautifier.beautify('\n'.join(js), opts)
    multiLine = []
    for line in js.split('\n'):
        multiLine.append(line)
    return multiLine

def read_js_file():
    try:
        with open(jsfile) as fd:
            return fd.read().split('\n')
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

def couchjs():
    if 'Object.keys' in js:
        print('File contains "Object.keys" which is not available until ECMA2015')
        exit(1)
    try:
        # couchjs_exe='~/local/git/apache/couchdb/bin/couchjs' # if couchjs isn't in your path
        couchjs_exe='couchjs'
        code = call([couchjs_exe, jsfile])
        if code != 0:
            print('couchjs found errors in your code.')
            exit(1)
    except Exception as e:
        print('failed running couchjs')
        raise e

print('Checking for errors:')
if (shutil.which('couchjs')):
    couchjs()
else:
    print('    couchjs not found, skipping...')

print('Replacing {}/{}'.format(os.path.splitext(os.path.basename(design_file))[0], view_name))

try:
    design_doc['views'][view_name][view_function] = multiline_view(js)
    data = json.dumps(design_doc, sort_keys=True, indent=4, separators=(",", ": "))
    wrote = open(design_file, 'w').write(data + '\n')
    print('Wrote {} bytes'.format(wrote))
except Exception as e:
    print('failed to repalce javascript for {}/{}'.format(view_name, view_function))
    print(e)
    exit(3)
