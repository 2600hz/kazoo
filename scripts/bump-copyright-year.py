#!/usr/bin/env python3

# bumps copyright year to current year in source/hrl files
# adds MPL2.0 license to each source file if missing

import re
import sys
import datetime
import os.path

mpl_license = """%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
"""

apa_license = "Licensed under the Apache License"

module_end = """%%% @end
%%%-----------------------------------------------------------------------------"""

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

def update_copyright(filename):
    found = get_copyright(filename)
    if found == None:
        return 0
    (line, contents) = found
    year = re.findall('20[0-9]{2}', line)[-1]
    new_line = line.replace(year, str(datetime.datetime.now().year))

    if line != new_line:
        new_contents = contents.replace(line, new_line)
        replace_line(filename, new_contents)
        return 1
    else:
        return 0

def update_license(filename):
    with open(filename, 'r') as fd:
        whole_doc = fd.read()

        apa_found = re.findall(apa_license, whole_doc)
        if len(apa_found) == 1:
            return 0 # skip Apache-licensed files

        found = re.findall(mpl_license, whole_doc, re.MULTILINE | re.DOTALL)

        if len(found) == 1:
            return 0

        found_end = re.findall(module_end, whole_doc, re.MULTILINE | re.DOTALL)
        if len(found_end) == 0:
            sys.stdout.write("\n{}:1: failed to find module header @end".format(filename))
            raise ValueError('no @end in ', filename)

        added = ''.join([mpl_license, module_end])

        updated = whole_doc.replace(str(found_end[0]), added, 1)

        with open(filename, 'w') as w:
            w.write(updated)
            return 1

replaced = 0
sys.stdout.write("checking copyright and license: ")
for filename in sys.argv[1:]:
    if (not os.path.isfile(filename)):
        continue

    basename, ext = os.path.splitext(filename)
    if (ext != ".erl"):
        continue

    updated_c = update_copyright(filename)
    updated_l = update_license(filename)
    if updated_c == 1 and updated_l == 1:
        sys.stdout.write("d")
    elif updated_c == 1:
        sys.stdout.write("c")
    elif updated_l == 1:
        sys.stdout.write("l")
    else:
        sys.stdout.write(".")

    replaced += (updated_c or updated_l)

print(" done")
