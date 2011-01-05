#!/bin/sh


cd `dirname $0`
exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/apps/*/ebin \
    -boot start_sasl -name whistle_apps -s whistle_apps \
    -mnesia dir '"priv/mnesia"' \
    -kernel error_logger '{file, "log/error_log"}'
