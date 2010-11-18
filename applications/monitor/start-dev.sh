#!/bin/bash
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/include -s monitor
