cd `dirname $0`

if [ ! -f "snmp.config" ] || [ ! -d "agent" ]  
then
	erl -pa ebin -pa ../../core/*/ebin -pa ../../deps/*/ebin -sname snmp -s whistle_snmp create_config -s init stop
fi

if [ ! -f "KAZOO-MIB.bin" ]
then
	erlc KAZOO-MIB.mib
fi

erl -pa ebin -pa ../../core/*/ebin -pa ../../deps/*/ebin -sname snmp -config snmp -s whistle_snmp -s stats
