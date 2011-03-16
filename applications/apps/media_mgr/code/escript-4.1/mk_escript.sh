#! /bin/bash
#---
# Excerpted from "Programming Erlang",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material, 
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose. 
# Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
#---
CWD=`pwd`
echo '#! /bin/bash' > escript
echo 'function escape() {' >> escript
echo '  for arg in $*; do echo \\${arg}; done' >> escript
echo '}' >> escript
echo 'args=`escape $*`' >> escript
echo 'erl -pa' $CWD '-boot start_clean -noshell -s escript start $0 $args' >> escript
chmod u+x escript

