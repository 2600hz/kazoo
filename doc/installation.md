# Kazoo Installation Guide

This is a guide to installing Kazoo on a Debian 8 (Jessie) base installation. Other GNU/Linux distros should work similarly, though the dependencies may differ a bit.

## Dependencies

### Packages Required

```shell
#> sudo apt-get install build-essential libxslt-dev \
    zip unzip expat zlib1g-dev libssl-dev curl \
    libncurses5-dev
```

### Erlang

Kazoo 4 targets Erlang 18+. There are a couple ways to install Erlang:

* From source. I prefer to use a tool like [kerl](https://github.com/yrashk/kerl) to manage my installations. If you want to play around with multiple versions of Erlang while hacking on Kazoo, this is probably the best way.
```shell
$ curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
$ chmod a+x kerl
$ mv kerl /usr/bin
$ kerl list releases
$ kerl build 18.2 r18.2 # this takes a while
$ kerl install r18.2 /usr/lib/erlang
$ . /usr/lib/erlang/activate
```

* Install from the [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html) packages. These tend to be kept up-to-date better than the default distro's packages.

## Building Kazoo

* Once the dependencies are all here, after a fresh `git clone https://github.com/2600Hz/kazoo.git` just `make`.
* When developing, one can `cd` into any app directory (within `applications/` or `core/`) and run:
    * `make` (`make all` or `make clean`)
    * `make xref` to look for calls to undefined functions (uses [Xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html))
    * `make dialyze` to statically type-check the app (uses [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html))
    * `make test` runs the app / sub-apps test suite, if any.
        * **Note:** make sure to `make clean all` after running your tests, as test BEAMs are generated in `ebin/`!
* To run the full test suite it is advised to
    1. `cd` into the root of the project
    1. `make compile-test` to compile every app with the `TEST` macro defined
    1. `make eunit` (instead of `make test`) to run the test suite without recompiling each app
    * *This way apps can call code from other apps that was compiled with the `TEST` macro defined*
* `make build-release` will generate a [deployable release](http://learnyousomeerlang.com/release-is-the-word)
    * [More on using releases with Kazoo](https://github.com/2600Hz/kazoo/blob/master/doc/engineering/releases.md)
* `make sup_completion` creates `sup.bash`: a Bash completion file for the SUP command

# Bigger Picture

## Server components

There are six major components to a Kazoo system, they can all be installed on one server or split arbitrarily over any number of servers.  This guide will provide examples for installing either a single server, or three server cluster.  The components and their functions are:

* [Kazoo](https://github.com/2600hz/kazoo)
  * Provides all application logic for the system, the brains of the operation.
* [RabbitMQ](https://github.com/rabbitmq/rabbitmq-server)
  * Messaging system using AMQP, it provides command and control as well as allows examination of running system state.
* [Kamailio](https://www.kamailio.org/w/)
  * Provides the SIP processing for the system.  For the purposes of this guide we will assume that it is always installed on the same server as Kazoo.
* [FreeSWITCH](https://freeswitch.org/)
  * Provides all media handling for calls.  In a multiple server cluster there will typically be dedicated FreeSWITCH servers.
* [CouchDB](https://couchdb.apache.org/)
  * Provides the database for configuration and reporting for a cluster.  Typically you will want CouchDB running on multiple servers in a cluster for redundancy purposes.
* [HAProxy](http://www.haproxy.org/)
  * Provides load balancing and request distribution.  Also routes internal system connections between components and CouchDB.
* [Monster-UI](https://github.com/2600hz/monster-ui/)
  * Provides the Kazoo web interface for configuration and monitoring of the system.

## Cluster design considerations

There are a few concerns that should be planned for when designing a Kazoo cluster.

* Design in enough CouchDB servers from the start
  * It is difficult to add additional database servers once the cluster has been deployed, as such you should plan in advance and add the maximum number of bigcouch servers that you forsee needing in the near future of the cluster from the start.
* Each zone should have it's own FreeSWITCH server
  * FreeSWITCH servers may not be shared between zones

## Pre-Installation

The following should be done to prepare a server for installation (this should be done on all servers in a cluster prior to installing anything)

### Add 2600hz repo
```
# wget -P /etc/yum.repos.d/ http://repo.2600hz.com/2600hz.repo
```

### Set correct IP / hostname

You must have the correct IP and host name configuration prior to starting installation.  It is highly reccomended (almost required) that you set a static IP for all servers that you are installing.

After setting a static IP, you must also configure the hostnames in /etc/hosts for both long and short hostnames.
Example:
```
191.168.1.100 test1.cluster1.2600hz.com test
```

### Disable firewall and SELinux

During installation you should disable any firewalls and SELinux, this is to prevent them from causing any problems during installation and initial testing.

```
# service iptables save && service iptables stop && chkconfig iptables off
```

Edit /etc/selinux/config (restart required)
```
SELINUX=disabled
```

### Verify time and date settings

It is important for the time and dates to be correct and in sync on all servers in a cluster.  It is highly reccomended that you use NTP to facilitate this.

Select the correct timezone
```
# tzselect
```

Symlink the timezone file to make the configuration change persistent
```
# ln -sf /usr/share/zoneinfo/<region>/<tzfile> /etc/localtime
```

Enable NTP
```
# service ntpd start && chkconfig ntpd on
```

Check date / time and verify it is correct
```
# date
```

## Single server installation

### Install packages

```
# yum install -y kazoo-bigcouch-R15B haproxy kazoo-freeswitch-R15B kazoo-kamailio kazoo-R15B httpd monster-ui*
```

### Configure packages

See Common Configuration section below for configuration that is to be done

### Single server specific configuration

The following configuration changes must be done in addition to the common configuration.

Edit /etc/kazoo/bigcouch/local.ini and update the cluster configuration values to the following:
```
q=1
r=1
w=1
n=1
```

Restart bigcouch for changes to take effect
```
# service bigcouch restart
```

You should now be ready to validate the installation

## Three server cluster installation

For the purposes of this guide we will assume the following cluster layout:

```
Server 1 (zone 1): bigcouch, kazoo
 Hostname: test1.cluster1.2600hz.com
 IP Addr : 192.168.1.100
```
```
Server 2 (zone 1): bigcouch, freeswitch
 Hostname: test2.cluster1.2600hz.com
 IP Addr : 192.168.1.101
```
```
Server 3 (zone 2): bigcouch, kazoo, monster-ui
 Hostname: test3.cluster1.2600hz.com
 IP Addr : 192.168.1.102
```
```
Server 4 (zone 2): bigcouch, freeswitch
 Hostname: test4.cluster1.2600hz.com
 IP Addr : 192.168.1.103
```

### Install and configure bigcouch

On all servers:
```
# yum install -y kazoo-bigcouch-R15B
```

**Set the Erlang cookie correctly for bigcouch (see section below)**

**Set up bigcouch cluster**
```
# curl -X PUT test1.cluster1.2600hz.com:5986/nodes/bigcouch@test2.cluster1.2600hz.com -d {}
# curl -X PUT test1.cluster1.2600hz.com:5986/nodes/bigcouch@test3.cluster1.2600hz.com -d {}
# curl -X PUT test1.cluster1.2600hz.com:5986/nodes/bigcouch@test4.cluster1.2600hz.com -d {}
```

### Install remaining packages

**Server 1**
```
# yum install -y kazoo-kamailio kazoo-R15B
```

**Server 2**
```
# yum install -y haproxy kazoo-freeswitch-R15B
```

**Server 3**
```
# yum install -y kazoo-kamailio kazoo-R15B httpd monster-ui*
```

**Server 4**
```
# yum install -y haproxy kazoo-freeswitch-R15B
```

### Configure packages

See Common Configuration section below for configuration to be done on all servers

### Cluster specific configuration

**Update HAProxy configuration with all bigcouch servers**
*/etc/kazoo/haproxy/haproxy.cfg*
```
listen bigcouch-data 127.0.0.1:15984
  balance roundrobin
    server test1.cluster1.2600hz.com 192.168.1.100:5984 check
    server test2.cluster1.2600hz.com 192.168.1.101:5984 check
    server test3.cluster1.2600hz.com 192.168.1.102:5984 check
    server test4.cluster1.2600hz.com 192.168.1.103:5984 check backup

listen bigcouch-mgr 127.0.0.1:15986
  balance roundrobin
    server test1.cluster1.2600hz.com 192.168.1.100:5986 check
    server test2.cluster1.2600hz.com 192.168.1.101:5986 check
    server test3.cluster1.2600hz.com 192.168.1.102:5986 check
    server test4.cluster1.2600hz.com 192.168.1.103:5986 check backup
```

You may want to have a separate configuration for each zone, with the other zone configured as backup's.

## Common Configuration

### Set Erlang cookies

All components must share the same Erlang cookie.  Since Erlang cookies allow unrestricted access to the Erlang VM you should use a unique and non-public cookie string.  The cookie must be set in the following configuration files:

**/etc/kazoo/bigcouch/vm.args**
```
-setcookie COOKIEHERE
```

**/etc/kazoo/freeswitch/autoload_configs/kazoo.conf.xml**
```
<param name="cookie" value="COOKIEHERE" />
```

**/etc/kazoo/config.ini (in multiple locations)**
```
cookie = COOKIEHERE
```

### Configure HAProxy

Symlink the Kazoo HAProxy configruation file
```
# rm -f /etc/haproxy/haproxy.cfg
# ln -s /etc/kazoo/haproxy/haproxy.cfg /etc/haproxy/
```

### Configure Kamailio

Update the following values in the /etc/kazoo/kamailio/local.cfg file

*Server 1*
```
#!substdef "!MY_HOSTNAME!test1.cluster1.2600hz.com!g"

#!substdef "!MY_IP_ADDRESS!192.168.1.100!g"

#!substdef "!MY_AMQP_URL!kazoo://guest:guest@192.168.1.100:5672!g"

#!substdef "!MY_WEBSOCKET_DOMAIN!2600hz.com!g"
```

*Server 3*
```
#!substdef "!MY_HOSTNAME!test3.cluster1.2600hz.com!g"

#!substdef "!MY_IP_ADDRESS!192.168.1.102!g"

#!substdef "!MY_AMQP_URL!kazoo://guest:guest@192.168.1.102:5672!g"

#!substdef "!MY_WEBSOCKET_DOMAIN!2600hz.com!g"
```

On both Server 1 and Server 3 update /etc/kazoo/kamailio/dbtext/dispatcher to contain the following:
```
1 sip:192.168.1.101:11000 0
```

### Configure Kazoo / RabbitMQ

We will now create 2 zones, one for each Kazoo server.  Edit the zone, whistle_apps, and ecallmgr sections of /etc/kazoo/config.ini to look like the following:
```
[zone]
name = "c1_zone1"
amqp_uri = "amqp://guest:guest@192.168.1.100:5672"

[zone]
name = "c1_zone2"
amqp_uri = "amqp://guest:guest@192.168.1.101:5672"

[whistle_apps]
host = "test1.cluster1.2600hz.com"
zone = "c1_zone1"
cookie = COOKIEHERE

[whistle_apps]
host = "test3.cluster1.2600hz.com"
zone = "c1_zone2"
cookie = COOKIEHERE

[ecallmgr]
host = "test1.cluster1.2600hz.com"
zone = "c1_zone1"
cookie = COOKIEHERE

[ecallmgr]
host = "test3.cluster1.2600hz.com"
zone = "c1_zone2"
cookie = COOKIEHERE
```
### Configure monster-ui

Edit /var/www/html/monster-ui/js/config.js and ensure the api default value is correctly set to either Server 1 or Server 3:
```
default: 'http://192.168.1.102:8000/v2/'
```

### Ensure services are running and set to auto-start

(Note: for single server install, combine instructions for Server 1 and Server 2)

Start all services

**Server 1**
```
# service bigcouch restart
# service rabbitmq-server restart
# service rsyslog restart
# service kz-whistle_apps restart
# service kz-ecallmgr restart
# service kamailio restart
# service httpd restart
```

**Server 2**
```
# service bigcouch restart
# service rsyslog restart
# service haproxy restart
# service freeswitch restart
```

**Server 3**
```
# service bigcouch restart
# service rabbitmq-server restart
# service rsyslog restart
# service kz-whistle_apps restart
# service kz-ecallmgr restart
# service kamailio restart
# service httpd restart
```

Enable auto-startup for all services

**Server 1**
```
# chkconfig --add rabbitmq-server
# chkconfig --add kz-ecallmgr
# chkconfig --add kz-whistle_apps
# chkconfig rabbitmq-server on
# chkconfig kz-ecallmgr on
# chkconfig kz-whistle_apps on
# chkconfig kamailio on
# chkconfig bigcouch on
# chkconfig httpd on
```

**Server 2**
```
# chkconfig haproxy on
# chkconfig freeswitch on
```

**Server 3**
```
# chkconfig --add rabbitmq-server
# chkconfig --add kz-ecallmgr
# chkconfig --add kz-whistle_apps
# chkconfig rabbitmq-server on
# chkconfig kz-ecallmgr on
# chkconfig kz-whistle_apps on
# chkconfig kamailio on
# chkconfig bigcouch on
# chkconfig httpd on
```

### Import media files

*Server 1 OR Server 3*
```
# sup whistle_media_maintenance import_prompts /opt/kazoo/system_media/en-us en-us
```

### Configure ecallmgr

Add freeswitch nodes
*Server 1*
```
# sup -n ecallmgr ecallmgr_maintenance add_fs_node freeswitch@test2.cluster1.2600hz.com
```

*Server 3*
```
# sup -n ecallmgr ecallmgr_maintenance add_fs_node freeswitch@test4.cluster1.2600hz.com
```

Add acl entries for SIP servers
*Server 1 OR Server 3*
```
# sup -n ecallmgr ecallmgr_maintenance allow_sbc test1.cluster1.2600hz.com 192.168.1.100
# sup -n ecallmgr ecallmgr_maintenance allow_sbc test3.cluster1.2600hz.com 192.168.1.102
```

## Validate installation

### Check CouchDB

On all servers, curl the database ip/port to verify that it is reachable:

```shell
#> curl localhost:5984
{"couchdb":"Welcome","version":"5fa9098","vendor":{"name":"The Apache Software Foundation"}}
```

### Check FreeSWITCH

Connect to the cli and verify that you have at least one profile running, also verify that BOTH ecallmgr nodes are connected
```
# fs_cli

> sofia status
< should show at least one profile>

> erlang nodes list
< should show BOTH ecallmgr nodes (Server1 and Server3)
```

### Check Kamailio status

*Server 1 and Server 3*
```
# kamctl fifo ds_list
```

### Check federation (for cluster installations)

*Server 1 and Server 3*
```
# service kz-whistle_apps status
```

Verify that the status shows nodes for BOTH Server 1 and Server 3

### Create master account

*Server 1 OR Server 3*
```
# sup crossbar_maintenance create_account {ACCT NAME} {REALM} {LOGIN} {PASSWORD}
```
### Load applications

*Server 1*
```
# sup crossbar_maintenance init_apps /var/www/html/monster-ui/apps/ http://192.168.1.100:8000/v2
```

*Server 3*
```
# sup crossbar_maintenance init_apps /var/www/html/monster-ui/apps/ http://192.168.1.102:8000/v2
```

## Notes / Credits

This guide was created using the (very good) guide at http://www.powerpbx.org/content/kazoo-v3-single-or-multiple-server-voip-telephony-platform-install-guide-v1 as a template / starting point.
