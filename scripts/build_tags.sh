#!/bin/sh
rm TAGS
find whistle_apps/ -name "*.erl"  | xargs etags --append
find ecallmgr/ -name "*.erl"  | xargs etags --append
find lib/whistle* -name "*.erl"  | xargs etags --append
find lib/braintree* -name "*.erl"  | xargs etags --append
