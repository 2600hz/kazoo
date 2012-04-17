#!/bin/bash

OPENSIPS="your.whapps.server.here.com other.whapps.server.if.any"
FREESWITCH="fs1.com fs2.com fs3.com fs4.com"
ECALLMGR=$OPENSIPS
RABBITMQ=$OPENSIPS
WHAPPS=$OPENSIPS
HAPROXY=$OPENSIPS
BIGCOUCH="db01.server.com db02.server.com db03.server.com db04.server.com"
CROSSBAR=$OPENSIPS
WEBSERVERS=$OPENSIPS


function select_zone {
	echo -n "Please select a zone you wish to manage [default=1]:"
	read ZONE
	echo "Zone $ZONE selected."
	echo
}

function gather_servers {
	# NOTE: I want to replace this with a curl command or other local file read to get a list of all theoretically configured servers.
	# Suggestions welcome.

	# Get the list of OpenSIPs servers

	# Get the list of FreeSWITCH servers

	# Get the list of RabbitMQ servers

	# Get the list of ECallmgr servers

	# Get the list of WhApps servers

	# Get the list of HaProxy servers

	# Get the list of BigCouch servers

	# Get the list of API URLs
echo ""
}

function check_active() {
	port=$1
	shift;
	servers=$@
	working=0
	failed=0

	for server in $servers
	do
		if nc -z $server $port > /dev/null 2>&1
		then
#			echo $server port $port is open/working
			working=$((working+1))
		else
#			echo $server port $port is not open
			failed=$((failed+1))
		fi
	done

	if [ $working -gt 0 ] && [ $failed -gt 0 ]; then
		result="  \Z2$working OK/\Z1$failed WARN"
	elif [ $working -gt 0 ]; then
		result="     \Z2$working ONLINE\Zn"
	else
		result="    \Z1$failed OFFLINE"
	fi

	echo "$result"
}

echo "Checking OpenSIPs..."
osips_status=$(check_active 5060 $OPENSIPS)
echo

echo "Checking FreeWITCH Nodes..."
frees_status=$(check_active 5060 $FREESWITCH)
#echo $(check_active 8031 $FREESWITCH)
echo

echo "Checking eCallMgr Nodes..."
# How can we do this?
ecall_status="      UNKNOWN"
echo

echo "Checking RabbitMQ Nodes..."
rabmq_status=$(check_active 5672 $RABBITMQ)
echo

echo "Checking WhApps Nodes..."
# How can we do this?
whapp_status="      UNKNOWN"
echo

echo "Checking HAProxy Availability..."
hapro_status=$(check_active 15984 $HAPROXY)
#echo $(check_active 15986 $HAPROXY)
echo

echo "Checking Database (BigCouch) Nodes..."
bigco_status=$(check_active 5984 $BIGCOUCH)
#echo $(check_active 5986 $BIGCOUCH)
echo

echo "Checking API Nodes..."
cross_status=$(check_active 8000 $CROSSBAR)
whapp_status=$cross_status
echo

echo "Checking Web Servers..."
httpd_status=$(check_active 80 $WEBSERVERS)
#echo $(check_active 443 $WEBSERVERS)
echo

echo "Checking Winkstart Status..."
winks_status="      UNKNOWN"
#
echo


# Need to add a check that OpenSIPs is connected to X nodes
# Need to add a check that FreeSWITCH has X ecallmgrs
# Need to add a check that of what whapps are running

title="Zone 1 (US) / Cluster 1 (Dallas)"

output="\n\
\Zn        SIP Stack Status                    2600hz Platform                   Web Service Status      \n\
\Zn--------------------------------   --------------------------------   --------------------------------\n\
\Zn OpenSIPs   =     $osips_status     eCallMgr  =      $ecall_status     HTTP Server  =   $httpd_status \n\
\Zn FreeSWITCH =     $frees_status     RabbitMQ  =      $rabmq_status     Winkstart    =   $winks_status \n\
\Zn Activity   =             CALLS     WhApps    =      $whapp_status     API Server   =   $cross_status \n\
\Zn Devices    =             REG'D     HaProxy   =      $hapro_status\n\
\Zn                                    BigCouch  =      $bigco_status\n\
\Zn\n
\n
"
#      Security Status              System Services\n\
#---------------------------   ---------------------------   ---------------------------\n\
# IPTables   =       ONLINE     Postfix   =        ONLINE\n\
# Fail2ban   =       ONLINE\n\
#"

dialog --colors --no-collapse --title "$title" \
	--msgbox "$output" \
        30 107

exit


# Goal: To display all elements below, from a single server
#

# Configured OpenSIPs Servers
# Connected OpenSIPs Servers
# Responding OpenSIPs Servers
# Running Version


# Configured FS Servers
# Connected FS Servers
#Responding FS Servers
#Run a FS Command
#Running Version


#Configured Whapps
#Running Whapps
#Running Version


#Configured Databases
#Connected Databases
#Running Database
#Running Version


# Goal: To allow access to these commands from the CLI:

# Submenu: System Config Maintenance
# 1) Clear System Cache  -- wh_cache:flush().
# 2) Reload System Configuration  -- whapps_config:flush().
# 3) Run system upgrade maintenance routine, after a version upgrade     (whapps_maintenance:migrate())
# 4) Check what Couch & AMQP we are actually connected to	amqp_mgr:get_host().


# Submenu: Application Maintenance
# 1) Running WhApps
# 2) Start a WhApp
# 3) Stop a WhApp      (whapps_controller:stop/start_app(xxx).)


# Submenu: Database Maintenance
# 1) Verify database connectivity on all nodes    (check curl http://db.server:5986/_membership all matches on all servers, and haproxy.cfg is working)
# 2) Add a new database node to the cluster       (curl again!)
# 3) Move an account to a different database server     (curl replication fanciness)
# 4) Backup an account      (curl again! should allow "all")
# 5) Restore an account     (curl again! should allow "all" but with warnings, lots of warnings)


# Submenu: Device Maintenance
# 1) View registered devices known by this machine       (registrar_maintenance:local_summary().)
#        Note: It is not required that this machine know about every device, only that at least one machine in the cluster knows a device
# 2) Check for registration of a specific device across the cluster
# 3) Flush device attribute cache       (callflow_maintenance:flush(). <- the attributes cache, like CID)
# 4) Flush device registration cache  {ok, Cache} = registrar_sup:cache_proc()


# Submenu: Active Call Management
# 1) Show Calls on entire cluster     (callflow_maintenance:show_calls().)
# 2) Show Calls on specific call management server      (#ecallmgr_maintenance:show_calls().)
# 3) callflow_maintenance:flush().    what does this do?


# Submenu: Number Management Maintenance
# 1) Rebuild inbound phone number database    -- whistle_number_manager_maintenance:reconcile().  (allow specific account)
# 2) Verify where an inbound phone number routes to     (stepswitch_maintenance:lookup_number("+14158867900").
# 3) Verify where an outbound phone number routes to    (stepswitch_maintenance:process_number("+12127229901").
# 4) Reload global number routes/resources              (stepswitch_maintenance:reload_resources().)
# 5) Clear in-memory cache of number routing for recently called numbers       (stepswitch_maintenance:flush())


# Submenu: API Maintenance
# 1) View API status
# 2) Generate API key
# 3) Start an API module (enabling API access for a specific command set)      (crossbar:start_mod(cb_noauthz).)
# 4) Stop an API module (disabling API access for a specific command set)      (crossbar:stop_mod(cb_noauthz).)


# Submenu: Account Maintenance
# 1) Create a new account
# 2) Check status of an account
# 3) Toggle account enabled/disabled
# 4) Rebuild views for an account        (whapps_maintenance:refresh(). or whapps_maintenance:refresh('account_id')



# Command Bridge:
# whapps_config:get(<<"whapps_controller">>, <<"whapps">>).
# kanderson: command_bridge whapps_config flush
# [7:20pm] kanderson: or if you were running it on your laptop...
# [7:20pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com whapps_config flush
# [7:20pm] kanderson: or to start crossbar
# [7:20pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com whapps_controller start_app crossbar
# [7:20pm] kanderson: see running apps
# [7:21pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com whapps_controller running_apps
# [7:21pm] kanderson: see what should be running
# [7:21pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com whapps_config whistle_apps whapps
# [7:21pm] kanderson: see what fs nodes should be connected
# [7:21pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com -n ecallmgr ecallmgr_config get fs_nodes
# [7:22pm] kanderson: OH sorry the previsous was wrong
# [7:22pm] kanderson: command_bridge -c whatever_the_cookie_is -h apps001-aa-ord.2600hz.com whapps_config get whistle_apps whapps
# [7:24pm] kanderson: actually just tried the listing of whistle apps that should be running (as config'd in the db) and it was still wrong, should have been
# [7:24pm] kanderson: ./command_bridge -c change_me whapps_config get whapps_controller whapps
# command_bridge wh_number_manager reconcile_number Number AccountId

command_bridge crossbar_maintenance create_account AccountName Realm Username Password

