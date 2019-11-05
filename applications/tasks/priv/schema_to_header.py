#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys, os, json

if len(sys.argv) != 3:
    print('Usage: ' + sys.argv[0] + ' /path/to/schema.json /path/to/module.hrl')
    exit(0)

def schema_to_header(schema, header_path):
    with open(header_path, 'w') as header_file:
        (name, ext) = (os.path.splitext(os.path.basename(header_path)))
        header_name = name.upper()
        keys = list(schema['properties'].keys())
        keys.sort()
        first, rest = keys[0], keys[1:]

        header_file.write("-ifndef("+header_name+"_HRL).\n")
        header_file.write("-define("+header_name+"_HRL, 'true').\n\n")
        header_file.write("-define(DOC_FIELDS, [<<\""+first+"\">>\n")
        for k in rest:
            header_file.write("                    ,<<\""+k+"\">>\n")
        header_file.write("                    ]).\n\n")

        required = schema.get('required', [])
        first, rest = required[0], required[1:]
        header_file.write("-define(MANDATORY_FIELDS, [<<\""+first+"\">>\n")
        for k in rest:
            header_file.write("                          ,<<\""+k+"\">>\n")
        header_file.write("                          ]).\n\n")
        header_file.write("-endif.\n")

with open(sys.argv[1], 'r') as schema_file:
    schema = json.load(schema_file)
    schema_to_header(schema, sys.argv[2])
