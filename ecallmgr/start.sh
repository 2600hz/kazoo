#!/bin/sh


cd `dirname $0`
<<<<<<< HEAD
export ERL_LIBS=$PWD/../lib/
=======
exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin $PWD/lib/*/ebin \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name ecallmgr@`hostname -f` -s ecallmgr
>>>>>>> master

exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -boot start_sasl -name ecallmgr@`hostname -f` -s ecallmgr \
    -kernel error_logger '{file, "log/error_log"}'
