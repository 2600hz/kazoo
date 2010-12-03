#!/bin/bash
erl -sasl errlog_type error -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/include -s monitor
