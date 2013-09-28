cd `dirname $0`

erl -pa ebin -pa ../../core/*/ebin -pa ../../deps/*/ebin -sname ipdevice -s ipdevice -detached
