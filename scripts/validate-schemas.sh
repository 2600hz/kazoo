#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Validate JSON schemas using python2's jsonschema tool

from __future__ import print_function
import json
import jsonschema
import os
import sys
from jsonschema.validators import validator_for


class MissingDefaultKeyError(Exception):
    """A 'default' key is missing from the JSON"""
class MissingTypeKeyError(Exception):
    """A 'type' key is missing from the JSON"""
def missing_default_or_type(json_file, current_key, JSON):
    print(json_file)
    keys = JSON.keys()
    print('at:', current_key)
    for key in keys:
        print('\t', key, ':', JSON[key])
    if 'default' not in keys:
        print(MissingDefaultKeyError.__doc__)
    if 'type' not in keys:
        print(MissingTypeKeyError.__doc__)
        # raise MissingTypeKeyError ## FIXME: uncomment this and fix the errors
def is_root(keys):
    return '$schema' in keys and '_id' in keys
def is_a_special_key(key):
    return key in ['properties', 'default_caller_id_number']
def has_a_special_key(keys):
    special_in = ['$ref', 'oneOf']
    return set() != set(keys).intersection(set(special_in))
class BadDefaultType(Exception):
    """default does not match type"""
def bad_default_type(json_file, current_key, Type, Default):
    print(json_file)
    print(BadDefaultType.__doc__)
    print('at:', current_key)
    print('type:', Type)
    print('default:', Default)
    raise BadDefaultType

def default_matches_type(Default, Type):
    if Type == 'object' and isinstance(Default, dict):
        return True
    if Type == 'boolean' and Default in [True, False]:
        return True
    if Type == 'integer' and isinstance(Default, int):
        return True
    if Type == 'number' and (isinstance(Default, int) or isinstance(Default, float)):
        return True
    if Type == 'array' and isinstance(Default, list):
        return True
    if Type == 'string' and isinstance(Default, unicode):
        return True
    if isinstance(Type, list):
        verifier = lambda t, acc: acc or default_matches_type(Default, t)
        return reduce(verifier, Type, False)
    return False

def check_defaults(json_file, current_key, JSON):
    keys = JSON.keys()
    if 'description' in keys and not is_root(keys):
        if 'default' not in keys or 'type' not in keys:
            if not is_a_special_key(current_key) and not has_a_special_key(keys):
                missing_default_or_type(json_file, current_key, JSON)
        else:
            Type = JSON['type']
            Default = JSON['default']
            if not default_matches_type(Default, Type):
                bad_default_type(json_file, current_key, Type, Default)
    for key in keys:
        value = JSON[key]
        if isinstance(value, dict):
            check_defaults(json_file, key, value)

def validate(json_file):
    with open(json_file) as fd:
        JSON = json.load(fd)
    check_defaults(json_file, '', JSON)
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
