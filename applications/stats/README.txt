The SNMP application uses whistle_stats.erl running on each node to collect
information, and whistle_stats_master.erl to aggregate the information for
SNMP. The items collected are based on KAZOO-200, and includes:

* Memory stats on each node - total, processes, system, atom, binary and code
and erlang version.
* Ecallmgr process count, number of reductions.
SIP events - number of registration attempts and failures, number of subscribes
* Then for each sip domain
Number of unallocated numbers, recovery on timer expire and progress timeouts.

The SNMP service is currently configured to run on port 4000, uses the
OID 1.3.6.1.4.1.700001 for Kazoo with the read only version 2c community string
"public". 

To test it, go to /opt/kazoo/applications/snmp and run start-dev.sh and make
sure the ecallmgr and whistle_apps nodes are running whistle_stats:start_link().
Then,
snmpwalk -v 2c -c public localhost:4000 1.3.6.1.4.1.700001

There are three modules - whistle_snmp.erl that provides the implementation
functions for the SNMP interface, whistle_stats.erl that runs on each node,
and whistle_stats_master.erl that collects the stats sent by each node, and
aggregates the data.

whistle_stats uses the routing key "statistics" on the "targeted" exchange to
send information updates to whistle_stats_master.

As per Karl's suggestion, the folsom module has been removed and information is
stored in the #state{} record. Also, it does not report rates as SNMP 
monitoring software such as MRTG, cactus and zabbix prefer raw values and can 
calculate average rates across different time periods, with nice graphs.

--- To do ---

Register a OID for Kazoo/2600hz - 700001 is unofficial.

Proper documentation and dialyzer information. Maybe need function name changes
to conform to 2600hz standards.

It would be nice if there was some way to report the Kazoo version. Is there
some way git can report the version checked out? Then a script could add this
to the code.

Implement IP based ACLs for access to SNMP.

Collect AMQP stats, and bigcouch response times.
