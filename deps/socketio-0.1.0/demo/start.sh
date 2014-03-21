#!/bin/sh
erl -sname demo -pa ebin -pa ../ebin ../deps/*/ebin -eval "demo:start()."
