#!/usr/bin/env python2

# print 'Usage: ' + sys.argv[0] + ' file.json+'

import sys
import json
from subprocess import call
import os

if len(sys.argv) < 2:
    pass

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
        else:
            print field, 'passed'
    finally:
        os.remove(TMP)

for fn in sys.argv[1:]:
    print 'checking ' + fn
    with open(fn) as rd:
        data = json.load(rd)
        fmap(couchjs, data)
