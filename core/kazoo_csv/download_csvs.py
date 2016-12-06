#!/usr/bin/env python2

import requests, sys, json

print("version: ", requests.__version__)
csvs_url = 'https://api.github.com/repos/maxogden/csv-spectrum/contents/csvs'
listing = requests.get(csvs_url).json()

headers = {'accept': 'application/vnd.github.v3.raw'}

if (isinstance(listing, list)):
    for remote_file in listing:
        print("fetching ", remote_file[u'url'])
        rf = requests.get(remote_file[u'url'], headers=headers)
        with open("test/"+remote_file[u'name'], 'wb') as fd:
            for chunk in rf.iter_content(chunk_size=128):
                fd.write(chunk)
else:
    print("failed to query", csvs_url, listing)
