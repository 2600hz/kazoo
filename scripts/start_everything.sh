#! /bin/bash

# Start Rabbit
start rabbitmq-server

# Start HAProxy
start haproxy

# Start BigCouch (alternative is "service bigcouch start")
bluepill load /etc/bluepill/bigcouch.pill
bluepill bigcouch start

# Start WhApps  (alternative is "service whapps start", bluepill just auto-restarted if there's an issue)
bluepill load /etc/bluepill/whapps.pill
bluepill whapps start

# Start FreeSWITCH (alternative is "service freeswitch start")
bluepill load /etc/bluepill/freeswitch.pill
bluepill freeswitch start

# Start Ecallmgr
bluepill load /etc/bluepill/ecallmgr.pill
bluepill ecallmgr start

echo "All done."
echo "Put your hostnames in /opt/kazoo/platform_status.sh at the top, and run it to make sure everything is really up. IGNORE anything that says UNKNOWN, that means it's not tracked yet."
