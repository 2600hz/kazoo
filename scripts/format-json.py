#!/usr/bin/env python3

# print 'Usage: ' + sys.argv[0] + ' file.json+'

import sys
import json
import shutil

if len(sys.argv) < 2:
    pass

json.encoder.FLOAT_REPR = str
for fn in sys.argv[1:]:
    fn2 = fn + '~'
    with open(fn) as fd:
        try:
            data = json.load(fd)
            data2 = json.dumps(data, sort_keys=True, indent=4, separators=(",", ": "))
        except ValueError as e:
            print(fn + ": " + str(e))
            exit(1)
        with open(fn2, 'w') as fd2:
            fd2.write(data2 + '\n')
    shutil.move(fn2, fn)
