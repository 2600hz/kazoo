#!/bin/sh
erl -pa ebin deps/*/ebin -s hello_world \
	-eval "io:format(\"Point your browser at http://localhost:8080~n\")."
