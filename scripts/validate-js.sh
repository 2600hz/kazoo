#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import sys
import json
from subprocess import call
import os
import shutil
import re

design_pattern = re.compile('^_design/')

if len(sys.argv) < 2:
    print 'Usage: ' + sys.argv[0] + ' file.json+'
    exit(0)

def fmap(F, data):
    if isinstance(data, dict):
        for key, value in data.iteritems():
            fmap(F, (key,value))
    elif isinstance(data, tuple):
        (key, value) = data
        if isinstance(value, basestring):
            if value.startswith('function'):
                F(data)
        elif isinstance(value, list) and len(value) > 0 \
             and isinstance(value[0], basestring) and value[0].startswith('function'):
            F(data)
        else:
            fmap(F, value)
    elif isinstance(data, list):
        for i, value in enumerate(data):
            fmap(F, (i,value))
    elif isinstance(data, int):
        pass

def couchjs((field, js)):
    JS = ''.join(js) + '\n'
    if 'Object.keys' in JS:
        print field, 'contains "Object.keys" which is not available until ECMA2015'
        exit(1)
    TMP = '_'
    with open(TMP, 'w') as wd:
        wd.write(JS)
        wd.close()
        try:
            # couchjs_exe='~/local/git/apache/couchdb/bin/couchjs' # if couchjs isn't in your path
            couchjs_exe='couchjs'
            code = call([couchjs_exe, TMP])
            if code != 0:
                print_field_js(field, js)
                exit(1)
        except Exception as e:
            print 'failed running: ', couchjs, TMP
            raise e
        finally:
            os.remove(TMP)

def print_field_js(field, js):
    print 'Key:', field
    print 'Code:'
    if list == type(js):
        for line in js:
            print line
    else:
        print js

def basename2(file_name):
    ## http://stackoverflow.com/a/678242/1418165
    return os.path.splitext(os.path.basename(file_name))[0]

def check_name(file_name, JSON_name):
    fname = basename2(file_name)
    # a hacky way when the app has views for multi database
    # but the name of the view is same and they have different views inside
    # for example: account-webhooks.json and wehooksdb-webgooks.json
    fname = fname.split('-')[-1]
    jname = JSON_name.split('/')[-1]
    if fname != jname:
        print 'File name {} does not match _id field!'.format(file_name)
        print '\t', fname, u' ≠ ', jname
        exit(1)

def get_app_cat_index(filename, exploded):
    for cat in ['applications', 'core']:
        try:
            appcat = exploded.index(cat)
        except ValueError:
            pass
    if appcat is None:
        print 'file {} is not in kazoo core or applications'.format(filename)
        exit(1)
    return appcat

def get_appname(filename, exploded):
    app_cat_index = get_app_cat_index(filename, exploded)
    return exploded[app_cat_index + 1]

def get_view_destinations(filename, data, app_name, view_name, all_view_destinations):
    """
    Check kazoo object of view:
    * should not have defined both database and classification
    * should have at least one view_map item
    * the view name should not end up in another view db or classification
    """
    view_destinations = []
    if 'kazoo' in data:
        kazoo_obj = data['kazoo']
    else:
        print 'kazoo object is missing from view {}'.format(filename)
        exit(1)

    if 'view_map' in kazoo_obj:
        view_map = kazoo_obj['view_map']
        if not isinstance(view_map, list) or len(view_map) <= 0:
            print 'kazoo.view_map should be a non empty list of object in file {}'.format(filename)
            exit(1)
    else:
        print 'view_map list is missing from view definition kazoo object {}'.format(filename)
        exit(1)

    for obj in view_map:
        if 'database' in obj and 'classification' in obj:
            print 'can not have both database and classification defined on same object in {}'.format(filename)
            exit(1)
        if 'database' in obj:
            db = obj['database']
            dbname_viewname = '{}-{}'.format(db, view_name)
            if dbname_viewname in all_view_destinations:
                print 'view {} in db {} has same name as view {}'.format(filename, db, all_view_destinations[dbname_viewname])
                exit(1)
            view_destinations.append(db)
            all_view_destinations['{}-{}'.format(db, view_name)] = filename
        if 'classification' in obj:
            classification = obj['classification']
            class_viewname = '{}-{}'.format(classification, view_name)
            if class_viewname in all_view_destinations:
                print 'view {} in db {} has same classification as view {}'.format(filename, classification, all_view_destinations[class_viewname])
                exit(1)
            view_destinations.append(classification)
            all_view_destinations['{}-{}'.format(classification, view_name)] = filename
    return view_destinations

def kz_check_views_destination(data, filename, exploded, view_definition_ids, all_view_destinations):
    """
    Check every view has correct kazoo object defined.
    Also check if the id of doc that we save in to system_data does not collide with each other.
    For example no view definition doc in system_data can have account-kazoo_apps-user id.
    """
    if design_pattern.match(data['_id']):
        app_name = get_appname(filename, exploded)
        if app_name == 'webseq':
            # it is bad to ignore by app name, but anyway webseq is special since we
            # don't know where its belong
            return
        view_name = data['_id'].split('/')[-1]
        view_destinations = get_view_destinations(filename, data, app_name, view_name, all_view_destinations)
        if len(view_destinations) < 1:
            print 'no database or classification is defined for {}'.format(filename)
            exit(1)
        elif len(view_destinations) == 1:
            view_def_id = '{}-{}-{}'.format(view_destinations[0], app_name, view_name)
        else:
            view_def_id = 'multi_db-{}-{}'.format(app_name, view_name)

        if view_def_id is None:
            print 'can not find destination of the view {}'.format(filename)
            exit(1)
        if view_def_id in view_definition_ids:
            print 'view {}({}) has same system_data db id of view {}'.format(view_def_id, filename, view_definition_ids[view_def_id])
            exit(1)
        view_definition_ids[view_def_id] = filename

exit_code = 0

def main():
    view_definition_ids = {}
    all_view_destinations = {}
    for fn in sys.argv[1:]:
        if not os.path.isfile(fn):
            print 'not a file: {}'.format(fn);
            continue
        if not fn.endswith('.json'):
            print 'not a JSON file: {}'.format(fn);
            continue
        exploded = fn.split(os.sep)
        if not 'couchdb' in exploded:
            continue
        if 'fixtures' in exploded:
            continue
        if 'swagger.json' in exploded:
            continue
        with open(fn) as rd:
            data = json.load(rd)
            check_name(fn, data['_id'])
            try:
                fmap(couchjs, data)
                kz_check_views_destination(data, fn, exploded, view_definition_ids, all_view_destinations)
            except Exception as e:
                print 'failed to process {}'.format(fn)
                print e
                global exit_code
                exit_code=1
    print(':: Listing of which view belongs to which database:')
    print(json.dumps(all_view_destinations, sort_keys=True, indent=4))
    print(':: Listing doc_id of view definitions in system_data database')
    print(json.dumps(view_definition_ids, sort_keys=True, indent=4))

main()
exit(exit_code)
