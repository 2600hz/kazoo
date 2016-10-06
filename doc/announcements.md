- [Announcements for Kazoo](#orgheadline29)
  - [Notices](#orgheadline2)
    - [Further Module Name Changes in Core](#orgheadline1)
  - [Versions](#orgheadline22)
    - [4.0](#orgheadline16)
    - [3.22](#orgheadline21)
  - [Upcoming](#orgheadline28)
    - [May 2016](#orgheadline27)


# Announcements for Kazoo<a id="orgheadline29"></a>

This file will serve as a reference point for upcoming announcements, both of the temporal nature (this library will be deprecated in 6 months) and version-specific (upgrading from X to Y will require A, B, and C).

## Notices<a id="orgheadline2"></a>

### Further Module Name Changes in Core<a id="orgheadline1"></a>

Over the years as the code base grew and new authors joined the team the core module naming has become increasingly inconsistent. In order to make module names predictable, remove obscure references (such as wht\\\_\*) and adhear to stronger coding standars we will be making these consistent. This will be the last refactor to core and as before the `scripts/wh-to-kz.sh` script will be extended to provide developers with a tool to refactor any modules they have created.

This final refactor has been contentious as we discussed the value a consistent naming scheme in core would bring. This has delayed the final phase of the renaming initiative but we are now preparing to preform this action. We feel that the benefits outweigh disadvantages and understand that once 4.0 has been released as stable will not have an oppertunaty to correct this until the next major version.

We hope that you agree and and are not inconvienced by this change. As always we are here to help or answer any questions! Thank you for your understaning.

## Versions<a id="orgheadline22"></a>

### 4.0<a id="orgheadline16"></a>

1.  Erlang Version Support

    Starting with Kazoo 4.0 Erlang support will target 18+ and will not be backward compatible with prior Erlang versions.

2.  Consistent naming (removal of references to whistle)

    All instances of whistle or variations such as wh&ensp;have been replaced with kazoo or kz\\\_. This will include all maintenance functions as well as init.d/systemd scripts. There is a script that can be used to assist with renaming and changing references in code `scripts/wh-to-kz.sh`.

3.  Number Manager

    Upgrading to 4.0 will shift number management from `core/whistle_number_manager` to `core/kazoo_number_manager`. The upgrade process *should* be seamless from the user's perspective, with some exceptions: \* API calls for attachments and port requests all go to `cb_port_requests`

4.  CouchDB

    Upgrading will change the way Kazoo interacts with CouchDB (including deprecating using BigCouch and recommending CouchDB!). For most operations, nothing will be noticeably different.

5.  Authorizing-ID

    Currently, inbound calls from carriers don't have a Authorizing-ID but if the device has a redirection the CDR will have the Authorizing-ID header. this will change in version 4 as we believe the inbound leg should not have the Authorizing-ID set, so the CDR for inbound call (leg a) will not have the Authorizing-ID. The b-leg will have the Authorizing-ID set to the device that redirect the call. Restrictions will still be applied based on the device that redirected the call.

6.  Auth Token

    The `token_auth` database always had a lot of pressure in it, with known problems in size due to the nature of BigCouch/CouchDB, requiring intensive use of compactor which also led to intensive disk usage and network communication for synchronizing the db cluster.

    We decided to move to a cryptography-based solution by adopting [JSON web tokens](https://jwt.io/) which will allow the drop of `token_auth` database. The generated token can be safely validated by any node without requiring the usage of a database-persisted token.

    As a consequence of this change, the auth token included in REST API responses has increased in size. If you were validating the length of the auth\_token key (or X-Auth-Token header in the response headers) you may need to remove or update that check (we recommend removing it!).

7.  WebHooks

    Webhook trigger for federated messages is dropped. we now require that at least one instance of the webhooks app is running in each zone for the webhook to be triggered.

    Added new option `include_subaccounts` which will process the hook for subaccount events.

8.  Blacklist changes

    In the 3.x series, a `raw_numbers` attribute was allowed, ostensibly to match anonymous caller IDs. This has been removed and a more proper solution has been put in place. You can still add anonymous caller ID to your blacklists, or you can configure the account/system to block them via the `block_anonymous_caller_id` key:

        # Account
        #> sup kapps_account_config set {ACCOUNT_ID} stepswitch block_anonymous_caller_id true
        # System
        #> sup kapps_config set_default stepswitch block_anonymous_caller_id true

9.  Voicemail Messages

    Starting with Kazoo 4.0 all new voicemail messages goes into modb. All Kazoo Administrators need to migrate their voicemail messages from Kazoo versio 3.22 to MODB. There is maintenance command for this transition. For more information about this change please see documentation for the new kazoo\\\_voicemail core application and crossbar voicemail documentation.

10. Removing socket.io support from Websockets

    The Blackhole application providing websocket support currently utilizes the socket.io client libraries. Due to the poor support for this server side in Erlang as well as the judgement that this provides little benifit it has been removed. The websockets now provide messaging without the socket.io overhead. More documentation will be available shortly. Please note that we still consider websockets beta functionality.

11. System media has been moved

    The media prompts that we used to keep in the code repository for historical reasons has been moved the [kazoo-sounds](https://github.com/2600hz/kazoo-sounds) repository.

12. Moved `cf_endpoint` to core

    The Callflow modules `cf_endpoint` and `cf_attributes` were used by multiple applications creating undesired dependencies between applications. This was done to avoid copy-pasting common code but is a clear indication that this functionality belongs in core. We have removed these modules from Callflows and moved them to <https://github.com/2600hz/kazoo/tree/master/core/kazoo\_endpoint>.

13. Moved `knm_sip` to inside `kzsip_uri`

    The `knm_sip` module shares a lot of functionality with the `kazoo_sip` core application. Its innards have thus been moved.

### 3.22<a id="orgheadline21"></a>

1.  FreeSWITCH 1.4.26+ / 1.6+

    To upgrade to Kazoo-FreeSWITCH 1.4.26+ or FreeSWITCH 1.6+ you must set the system\\\_config/ecallmgr parameter `use_kazoo_dptools` to TRUE. Failure to do so may cause inconsistent channel information among eCallMgr nodes connected to the same FreeSWITCH instance. If you are using Kazoo-FreeSWITCH 1.4.x builds prior to .26 this parameter should be left as its default, FALSE.

2.  Monster UI Enabled Applications

    The enabled Monster UI applications were moved from an object on the account document to its own document with the id `apps_store`. When you run `sup kapps_maintenance migrate` this will automatically preform this operation but until it is complete users logging into Monster UI may not see their previously enabled applications. Once the migration is complete the change should be transparent to end-users.

3.  Company Directory PDF

    If you plan to support the new API to download the company directory as a PDF you will need to install `htmldoc` on any server running crossbar.

4.  Default WebRTC Port change

    The default ports that Kamailio listens to for the WebRTC websocket have changed, this was due to a port conflict on all-in-one installs with TLS enabled Kazoo APIs. The standard HTTP websocket port was 8080 and is now 5064. The TLS HTTP websocket port was 8443 and is now 5065. If you would like to continue using the old ports please update "/etc/kazoo/kamailio/local.cfg" after an update to kazoo-configs 3.22.12+

## Upcoming<a id="orgheadline28"></a>

### May 2016<a id="orgheadline27"></a>

1.  Deprecating `deps/mochiweb`

    Most operations have been moved to the Cowboy or Cowlib projects. We will formally remove mochiweb from `deps/`. If you maintain code apart from Kazoo that uses mochiweb, please either covert to equivalent functionality with Cowboy/Cowlib or plan how you'll build your custom code with your own dependency of mochiweb.

2.  Deprecating `deps/exmpp`

    exmpp library has problems restarting. it will be replaced by `deps/escalus`

3.  Deprecating `deps/ejson`

    ejson used for json encode, decode will be replaced by `deps/jiffy`

4.  Deprecating `deps/ibrowse`

    `ibrowse` will be replaced by `core/kazoo_web/kz_http` which is using Erlang `httpc`. `kz_http` is the new HTTP client module now and the previous `kz_http` module is renamed to `kz_http_util`.

    If you maintain code apart from Kazoo that uses `ibrowse`, please either covert to equivalent functionality with `kz_http=/=httpc` or plan how you'll build your custom code with your own dependency of `ibrowse`.
