/*
Section: Pivot
Title: Conferencing
Language: en-US
Version: 3.18
*/

# Overview

There are two main ways to add the caller to a conference:

1. Use pre-built Kazoo conference rooms, configured via Crossbar
2. Create ad-hoc conference rooms via Pivot

## Pre-built example

    {"module":"conference"
     ,"data":{"id":"conference_id"}
    }

## Ad-Hoc example

    {"module":"conference"
     ,"data":{
         "config":{
             "name":"My Ad-hoc Conference"
         }
     }
    }

This will create a minimalist conference bridge, named "My Ad-hoc Conference". Use the same name to ensure callers end up in the same conference together. The `config` object will be validated against the [conference JSON schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/conferences.json).
