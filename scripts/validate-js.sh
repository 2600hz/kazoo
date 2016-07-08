#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import sys
import json
from subprocess import call
import os

if len(sys.argv) < 2:
    print 'Usage: ' + sys.argv[0] + ' file.json+'
    exit(0)

def fmap(F, data):
    if isinstance(data, dict):
        for key, value in data.iteritems():
            fmap(F, (key,value))
    elif isinstance(data, tuple):
        (key, value) = data
        if isinstance(value, basestring):
            if value.startswith('function'):
                F(data)
        else:
            fmap(F, value)
    elif isinstance(data, list):
        for i, value in enumerate(data):
            fmap(F, (i,value))
    elif isinstance(data, int):
        pass


def couchjs((field, js)):
    TMP = '_'
    with open(TMP, 'w') as wd:
        wd.write(js + '\n')
    try:
        code = call(['couchjs', TMP])
        if code != 0:
            print 'Key:', field
            print 'Code:', js
            exit(1)
    finally:
        os.remove(TMP)


def basename2(file_name):
    ## http://stackoverflow.com/a/678242/1418165
    return os.path.splitext(os.path.basename(file_name))[0]

def check_name(file_name, JSON_name):
    fname = basename2(file_name)
    jname = JSON_name.split('/')[-1]
    if fname != jname:
        print 'File name does not match _id field!'
        print '\t', fname, u' ≠ ', jname
        exit(1)


for fn in sys.argv[1:]:
    if not os.path.isfile(fn):
        continue
    if not fn.endswith('.json'):
        continue
    exploded = fn.split(os.sep)
    if not 'couchdb' in exploded:
        continue
    if 'fixtures' in exploded:
        continue
    if 'swagger.json' in exploded:
        continue
    print 'checking ' + fn
    with open(fn) as rd:
        data = json.load(rd)
        check_name(fn, data['_id'])
        fmap(couchjs, data)
