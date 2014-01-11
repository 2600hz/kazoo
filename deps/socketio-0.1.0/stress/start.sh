#!/bin/sh
erl -config stress_test -sname stress_test -pa ebin -pa ebin deps/*/ebin ../ebin  -eval "stress_test:start()."
