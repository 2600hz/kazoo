#!/bin/bash

echo "Building PLT, may take a few minutes"
dialyzer --build_plt --apps kernel stdlib erts inets eunit deps/qdate deps/jsx

dialyzer ebin
