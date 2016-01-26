# Announcements for Kazoo

This file will serve as a reference point for upcoming announcements, both of the temporal nature (this library will be deprecated in 6 months) and version-specific (upgrading from X to Y will require A, B, and C).

## Versions

### 3.22

#### FreeSWITCH 1.4.26+ / 1.6+

To upgrade to Kazoo-FreeSWITCH 1.4.26+ or FreeSWITCH 1.6+ you must set the system_config/ecallmgr parameter `use_kazoo_dptools` to TRUE.  Failure to do so may cause inconsistent channel information among eCallMgr nodes connected to the same FreeSWITCH instance.  If you are using Kazoo-FreeSWITCH 1.4.x builds prior to .26 this parameter should be left as its default, FALSE.

#### Monster UI Enabled Applications

The enabled Monster UI applications were moved from an object on the account document to its own document with the id `apps_store`.  When you run `sup whapps_maintenance migrate` this will automatically preform this operation but until it is complete users logging into Monster UI may not see their previously enabled applications.  Once the migration is complete the change should be transparent to end-users.

#### Company Directory PDF

If you plan to support the new API to download the company directory as a PDF you will need to install 'htmldoc' on any server running crossbar.

#### Default WebRTC Port change

The default ports that Kamailio listens to for the WebRTC websocket have changed, this was due to a port conflict on all-in-one installs with TLS enabled Kazoo APIs. The standard HTTP websocket port was 8080 and is now 5064.  The TLS HTTP websocket port was 8443 and is now 5065.  If you would like to continue using the old ports please update "/etc/kazoo/kamailio/local.cfg" after an update to kazoo-configs 3.22.12+

### 4.0

#### Erlang Version Support

Starting with Kazoo 4.0 Erlang support will target 18+ and will not be backward compatible with prior Erlang versions.

#### Number Manager

Upgrading to 4.0 will shift number management from `core/whistle_number_manager` to `core/kazoo_number_manager`. The upgrade process *should* be seamless from the user's perspective.

#### CouchDB

Upgrading will change the way Kazoo interacts with CouchDB (including deprecating using BigCouch and recommending CouchDB!). For most operations, nothing will be noticeably different.

## Upcoming

### May 2016

#### Deprecating `deps/mochiweb`

Most operations have been moved to the Cowboy or Cowlib projects. We will formally remove mochiweb from `deps/`. If you maintain code apart from Kazoo that uses mochiweb, please either covert to equivalent functionality with Cowboy/Cowlib or plan how you'll build your custom code with your own dependency of mochiweb.

#### Deprecating `deps/exmpp`

exmpp library has problems restarting. it will be replaced by `deps/escalus`

#### Deprecating `deps/ibrowse`
`ibrowse` will be replaced by `core/whistle_web/kz_http` which is using Erlang `httpc`. `kz_http` is the new HTTP client module now and the previous `kz_http` module is renamed to `kz_http_util`.

If you maintain code apart from Kazoo that uses `ibrowse`, please either covert to equivalent functionality with `kz_http`/`httpc` or plan how you’ll build your custom code with your own dependency of `ibrowse`.