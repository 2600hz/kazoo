#!/usr/bin/env python2

# bumps copyright year to current year in source/hrl files

import re
import sys
import datetime

def get_copyright(filename):
    with open(filename, 'r') as fd:
        ref = fd.read()
        found = re.findall('copyright.+2600Hz', ref)
        if len(found) == 1:
            [line] = found
            return (line, ref)
        else:
            return None

def replace_line(filename, new_contents):
    with open(filename, 'w') as fd:
        fd.write(new_contents)

replaced = 0
for filename in sys.argv[1:]:
    found = get_copyright(filename)
    if found == None:
        continue
    (line, contents) = found
    year = re.findall('20[0-9]{2}', line)[-1]
    new_line = line.replace(year, str(datetime.datetime.now().year))
    if line != new_line:
        if replaced == 0:
            sys.stdout.write("updating copyright: ")
        replaced += 1
        sys.stdout.write(".")
    new_contents = contents.replace(line, new_line)
    replace_line(filename, new_contents)

if replaced > 0:
    print " done"
