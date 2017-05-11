#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Validate JSON schemas using python2's jsonschema tool

from __future__ import print_function
import json
import jsonschema
import os
import sys
from jsonschema.validators import validator_for


def validate(json_file):
    with open(json_file, 'r') as fd:
        JSON = json.load(fd)
    try:
        validator = validator_for(JSON)
        validator.check_schema(JSON)
    except jsonschema.exceptions.SchemaError as e:
        print('Bad schema:', json_file)
        with open(json_file, 'r') as fd:
            print(fd.read())
        print(e)
        print()
        print('Run again with:')
        print(sys.argv[0], json_file)
        sys.exit(2)

for arg in sys.argv[1:]:
    if os.path.isdir(arg):
        for filename in os.listdir(arg):
            json_file = os.path.join(arg, filename)
            validate(json_file)
    elif os.path.exists(arg):
        validate(arg)
    else:
        print('Skipping', arg)
