#!/usr/bin/env python3

# Replaces catch clauses that use get_stacktrace() with the ?STACKTRACE(Class, Reason, Stacktrace)
# macro in kz_types.hrl

import os
import re
import sys

if len(sys.argv) < 2:
    print('Usage: ' + sys.argv[0] + ' path/to/file.erl', file=sys.stderr)
    sys.exit(1)

regex=r'(\s+)([\'\w_]+):([\'\w_]+)\s+\-\>\n\s+([A-Z_][\w_]+)([\s=]+erlang:get_stacktrace\(\),\n)'
replace=r'\1?STACKTRACE(\2, \3, \4)\n'

def match_stacktraces(whole_doc):
    return re.sub(regex, replace, whole_doc, re.MULTILINE | re.DOTALL)

errors=0
for erl in sys.argv[1::]:
    if not os.path.isfile(erl):
        print("Failed to find file ", erl, file=sys.stderr)
        continue

    with open(erl, 'r') as r:
        whole_doc = r.read()
        replaced = match_stacktraces(whole_doc)

        if whole_doc != replaced:
            with open(erl, 'w') as w:
                w.write(replaced)
                w.close()

        r.close()

sys.exit(errors)
