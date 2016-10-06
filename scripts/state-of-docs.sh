#!/usr/bin/env python2

# Find undocumented API endpoints

from glob import glob
import re
import sys

print 'Undocumented API endpoints:'

def endpoints(wildcard_path):
    APIs = set([])
    for fn in glob(wildcard_path):
        with open(fn, 'r') as fd:
            ref = fd.read()
            APIs = set.union(APIs, re.findall(r'> [A-Z]+ [^?\s\n]+', ref))
    return APIs

def sort_endpoints(APIs):
    return sorted(APIs, key=lambda x: x.split('}', 1)[-1])

APIs = endpoints('applications/crossbar/doc/ref/*.md')
MDs = endpoints('applications/crossbar/doc/*.md')

Wrong = set.difference(MDs, APIs)
Undocumented = set.difference(APIs, MDs)
Documented = set.intersection(APIs, MDs)
for API in sort_endpoints(Undocumented):
    print API

wrong = len(Wrong)
undocumented = len(Undocumented)
documented = len(Documented)

total = documented + undocumented
percent_documented = documented * 100 / total
print
print documented, '/', total, '(', str(percent_documented) + '% documented', ')'

if 0 != wrong:
    print
    print 'Documented but not matching any allowed_method:'
    for API in sort_endpoints(Wrong):
        print API
    #sys.exit(wrong)

sys.exit(100 - percent_documented)
