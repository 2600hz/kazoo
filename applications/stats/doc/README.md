
Events counts are collected by kazoo_stats.erl running on every kazoo
cluster node and are regularly sent via the targeted/statistics amqp queue to
the stats application. The stats application can be queried using SNMP.
There are 3 SNMP tables, each reporting information grouped by
1. Node/VM. This is for items common to all nodes
2. Ecallmgr. This table reports ecallmgr events grouped by ecallmgr node.
3. SIP domain. Reports events grouped by SIP domains/accounts.

The items collected are based on KAZOO-200, and includes:

* Memory stats on each node - total, processes, system, atom, binary and code
and erlang version. Number of AMQP requests and errors. Number of BigCouch
errors (error code 504 and others).
* Ecallmgr process count, number of reductions, number of registration
attempts and failures, number of subscribes
* Then for each sip domain, numbers of successful/unsuccessful call attempts
based on the CDR Hangup-cause.

The SNMP service is currently configured to run on port 4000, uses the
OID 1.3.6.1.4.1.700001 for Kazoo with the read only version 2c community string
"public".

To test it, go to /opt/kazoo/applications/stats and run start-dev.sh. On each
node, run kazoo_stats_sup:start_link().
snmpwalk -v 2c -c public localhost:4000 1.3.6.1.4.1.700001

There are three modules - kazoo_snmp.erl that provides the implementation
functions for the SNMP interface, kazoo_stats.erl that runs on each node,
and stats_handler.erl that collects the stats sent by each node, and
aggregates the data.


To-do:

Update the OID number 700001 to the registered number for Kazoo/2600Hz
Implement IP based ACLs for access to SNMP.
