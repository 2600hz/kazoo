#!/usr/bin/env python2

# Find undocumented API endpoints

from glob import glob
import re
import sys

print 'Undocumented API endpoints:'

APIs = []
for fn in glob('applications/crossbar/doc/ref/*.md'):
    with open(fn, 'r') as fd:
        ref = fd.read()
        endpoints = re.findall('> [^\n]+', ref)
        APIs.extend(endpoints)

MDs = []
for fn in glob('applications/crossbar/doc/*.md'):
    with open(fn, 'r') as fd:
        MDs.append(fd.read())

documented, undocumented = 0, 0
for endpoint in APIs:
    found = False
    for MD in MDs:
        if '\n'+endpoint+'\n' in MD:
            found = True
            break
    if found:
        documented += 1
    else:
        print endpoint
        undocumented += 1

total = documented + undocumented
percent_documented = documented * 100 / total
print documented, '/', total, '(', str(percent_documented) + '% documented', ')'
sys.exit(100 - percent_documented)
