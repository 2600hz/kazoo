#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
from subprocess import call
import json
import jsbeautifier
import shutil
import argparse, textwrap

banner = '''
╻┏ ┏━┓╺━┓┏━┓┏━┓   ╻ ╻╻┏━╸╻ ╻
┣┻┓┣━┫┏━┛┃ ┃┃ ┃   ┃┏┛┃┣╸ ┃╻┃
╹ ╹╹ ╹┗━╸┗━┛┗━┛   ┗┛ ╹┗━╸┗┻┛(o_O)
Extract or replace JavaScript inside a design document file.
You know when you need this!
'''

separator = '+'

parser = argparse.ArgumentParser(
    formatter_class=argparse.RawDescriptionHelpFormatter,
    description=textwrap.dedent(banner)
)
parser.add_argument('action', help='action to do', choices=['extract', 'extract_all', 'replace', 'replace_all'])
parser.add_argument('design_file', help='path to design document')
path_group = parser.add_mutually_exclusive_group()
path_group.add_argument('--js-dir',
                        help='path to a directory containing view codes; Default: current directory.", \
                        " File names should be like like "design_name{}view_name{}fun.js". "fun" indicates map or reduce function' \
                        .format(separator, separator, separator)
                        )
path_group.add_argument('--js-file', help='path to JavaScript file')
group1 = parser.add_argument_group('extract/replace a single view options', 'extract/replace an individual view in design doc')
group1.add_argument('--view-name', help='the name of the view that JavaScript belong')
group1.add_argument('--view-function'
                    ,help='view function name to replace its JavaScript, i.e. map or reduce; Default: map'
                    ,default='map'
                    ,choices=['map', 'reduce']
                    )

prog_args = parser.parse_args()
design_file = prog_args.design_file
design_name = os.path.splitext(os.path.basename(design_file))[0]

def print_help():
    parser.print_help()
    exit(1)

def view_js_filename(name, is_reduce):
    with_fun = 'reduce' if is_reduce else 'map'
    return design_name + separator + name + separator + with_fun + '.js'

def normalize_view_name(string):
    return \
        string.lstrip(design_name) \
        .replace('.js', '') \
        .replace(separator + 'reduce', '') \
        .replace(separator + 'map', '') \
        .replace(separator, '')

def is_reduce_file(string):
    return string.endswith(separator + 'reduce.js')

if prog_args.action == 'extract' or prog_args.action == 'replace':
    if not prog_args.view_name:
        print('error: the following argument is required: --view-name')
        print_help()
elif prog_args.view_name:
    if prog_args.action == 'extract_all':
        prog_args.action = 'extract'
    elif prog_args.action == 'replace_all':
        prog_args.action = 'replace'
    else:
        prog_args.action = 'extract'

if not prog_args.js_dir:
    cwd = os.getcwd()
    prog_args.js_dir = cwd
    if prog_args.action == 'extract_all' or prog_args.action == 'replace_all':
        print('using {} as js-dir'.format(prog_args.js_dir))
if not prog_args.js_file and (prog_args.action == 'extract' or prog_args.action == 'replace'):
    filename = view_js_filename(prog_args.view_name,
                                prog_args.view_function == 'reduce')
    prog_args.js_file = os.path.join(prog_args.js_dir, filename)
    print('using {} as js-file'.format(prog_args.js_file))

def multiline_view(data):
    opts = jsbeautifier.default_options()
    opts.indent_size = 2

    if type(data) is str:
        return data
    elif type(data) is list:
        ## We split and join new lines here to flatten js code into a single line
        ## so we can test that it would evaluated to a valid js code
        ## and it doesn't missed a semicolon `;`
        ## New line in jsbeautifier is for making comment line
        ## show on their own line
        js = jsbeautifier.beautify('\n'.join(data), opts)
        multiLine = []
        for line in js.split('\n'):
            multiLine.append(line)
        return multiLine
    else:
        print('bad js {}'.format(data))
        exit(3)

def read_js(file):
    try:
        ## We split and join new lines here to flatten js code into a single line
        ## so we can test that it would evaluated to a valid js code
        ## and it doesn't missed a semicolon `;`
        with open(file) as fd:
            data = fd.read().strip()
            if data.startswith('function'):
                return data.split('\n')
            else:
                return data
    except Exception as e:
        print('failed to read {}'.format(file))
        print(e)
        exit(1)

def read_json(file):
    try:
        with open(file) as fd:
            return json.load(fd)
    except Exception as e:
        print('failed to read {}'.format(file))
        print(e)
        exit(2)

def save_js(js_file, data):
    opts = jsbeautifier.default_options()
    opts.indent_size = 4
    js = ''.join(data).strip()
    if js.startswith('function'):
        ## New line in jsbeautifier is for making comment line
        ## show on their own line
        js = jsbeautifier.beautify('\n'.join(data), opts)
    try:
        open(js_file, 'w').write(js + '\n')
    except Exception as e:
        print('failed to save {}'.format(js_file))
        print(e)
        exit(3)

def save_views(views):
    try:
        design_doc = read_json(design_file)
        design_doc['views'] = views
        data = json.dumps(design_doc, sort_keys=True, indent=4, separators=(",", ": "))
        #print(data)
        wrote = open(design_file, 'w').write(data + '\n')
        print('Wrote {} bytes'.format(wrote))
    except Exception as e:
        print('failed to replace views in design document {}'.format(design_name))
        print(e)
        exit(3)

def couchjs(js):
    if type(js) is str:
        return
    ## we split and join new lines here to flatten js code into a single line
    ## so we can test that it would evaluated to a valid js code
    ## and it doesn't missed a semicolon `;`
    JS = ''.join(js) + '\n'
    if 'Object.keys' in JS:
        print(field, 'contains "Object.keys" which is not available until ECMA2015')
        exit(1)

    if not shutil.which('couchjs'):
        return

    TMP = '_'
    with open(TMP, 'w') as wd:
        wd.write(JS)
        wd.close()
        try:
            # couchjs_exe='~/local/git/apache/couchdb/bin/couchjs' # if couchjs isn't in your path
            couchjs_exe='couchjs'
            code = call([couchjs_exe, TMP])
            if code != 0:
                print('couchjs found errors in your code.')
                exit(1)
        except Exception as e:
            print('failed running couchjs')
            raise e
        finally:
            os.remove(TMP)

## replace
if prog_args.action == 'replace':
    print('Replacing {}/{}:'.format(design_name, prog_args.view_name, prog_args.view_function))
    js = read_js(prog_args.js_file)
    couchjs(js)
    design_doc = read_json(design_file)
    design_doc['views'][prog_args.view_name][prog_args.view_function] = multiline_view(js)
    save_views(design_doc['views'])
## replace_all
elif prog_args.action == 'replace_all':
    print('Replacing all views for {}'.format(design_name))
    views = {}
    for file in os.listdir(prog_args.js_dir):
        if file.startswith(design_name + separator) and file.endswith('.js'):
            view_name = normalize_view_name(file)
            if not view_name:
                print('bad view name for {}'.format(file))
                exit(3)
            view_function = 'reduce' if is_reduce_file(file) else 'map'

            print(':: replacing {}/{}:{}'.format(design_name, view_name, view_function))
            js = read_js(file)
            couchjs(js)
            if not view_name in views:
                views[view_name] = {}
            views[view_name][view_function] = multiline_view(js)
    if views:
        save_views(views)
    else:
        print('no javscript to replace')
## extract
elif prog_args.action == 'extract':
    print(':: extracting {}/{}:{}'\
          .format(design_name, prog_args.view_name, prog_args.view_function)
          )
    design_doc = read_json(design_file)
    if not prog_args.view_name in design_doc['views']:
        print('error: {} view not found in {}'\
              .format(prog_args.view_name, design_file)
              )
        exit(3)
    if not prog_args.view_function in design_doc['views'][prog_args.view_name]:
        print('error: function {} not found in {}/{}'\
              .format(prog_args.view_function, design_name, prog_args.view_name)
              )
        exit(3)
    save_js(prog_args.js_file,
            design_doc['views'][prog_args.view_name][prog_args.view_function]
            )
# extract_all
elif prog_args.action == 'extract_all':
    print('Extracting all views from {}'.format(design_file))
    design_doc = read_json(design_file)
    if not os.path.exists(prog_args.js_dir):
        os.makedirs(prog_args.js_dir)
    for view, view_funs in design_doc['views'].items():
        if 'map' in view_funs:
            print(':: extracting {}/{}:map'.format(design_name, view))
            save_js(os.path.join(prog_args.js_dir,
                                 view_js_filename(view, False)), view_funs['map']
                    )
        if 'reduce' in view_funs:
            print(':: extracting {}/{}:reduce'.format(design_name, view))
            save_js(os.path.join(prog_args.js_dir,
                                 view_js_filename(view, True)), view_funs['reduce']
                    )
