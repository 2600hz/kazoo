#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s winkstart

exec erl -detached -heart -setcookie `cat ../../confs/fs_conf/autoload_configs/.erlang.cookie` -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -boot start_sasl -name winkstart -s reloader -s winkstart \
    -kernel error_logger '{file, "log/error_log"}'
