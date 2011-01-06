#!/bin/sh


cd `dirname $0`
<<<<<<< HEAD
export ERL_LIBS=$PWD/../lib/

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin \
=======
exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin $PWD/lib/*/ebin \
>>>>>>> master
    -boot start_sasl -name ecallmgr@`hostname -f` -s ecallmgr
#    -kernel error_logger '{file, "log/error_log"}' \

