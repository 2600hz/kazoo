
# Call Inspector

A call is a back and forth exchange of SIP packets with more than one server at a time.
In case a call does not come through it can be tiresome to investigate of all those packets
of data, synchronized on different clocks.

Call Inspector enables you to list these SIP packets in order and, combined with the UI app,
display the exchange between the different legs into a ladder diagram.

## Components

The call_inspector application retrieves SIP packets in two ways:
1. by parsing FreeSWITCH and/or Kamailio log files (slower)
    * Note: if you want this app to parse Kamailio logs, they have to have the following format (note the timestamp formatting and [the presence of **cseq**](http://www.kamailio.org/wiki/cookbooks/4.0.x/pseudovariables#cseq))
    ```
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: UNIX0soi900etthorbq57j|start|received ws request REGISTER sip:wefwefwefwef.2600hz.com
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|source 192.168.11.43:33278
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|from sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|to sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|cseq 89
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|originated from external sources
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|this is a websocket request
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|correcting NATed contact in registration
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|log|authenticating sip:user_wpxnx7am9w@wefwefwefwef.2600hz.com via cached SIP creds
    Oct  4 20:58:22 wef kamailio[7421]: INFO: <script>: uncj0soi900etthorbq57j|end|successful registration with contact <sip:7l7upikd@192.168.11.43:33278;transport=ws;fs_path=<sip:192.168.11.50:5060;lr;received='sip:192.168.11.43:33278;transport=ws'>>
    ```
1. by listening for [HEP packets](https://2600hz.atlassian.net/wiki/display/docs/Homer+and+Kazoo#HomerandKazoo-C.haveKamailioandorFreeswitchcapture.) sent directly by FreeSWITCH and/or Kamailio

The packets are then stored by **call-id** in plain text files that can be log-rotated.
These files are stored in `/var/log/kazoo/call_inspector/{ID}` where `{ID}` is
an md5 hash of the **call-id**.

At this point the packets inside these files are either **chunks** or **analysis**.

### Chunks

A **chunk** is the app's representation of a SIP packet.
It contains the decoded packet and extracted information such as **call-id**,
source & destination port & IP address, timestamps (one set by the originating node and one by
the app's), and more.

A chunk in JSON format looks like this:

    {
      "call-id": "5dca43e524c680cf-13867@10.26.0.182",
      "dst": "10.26.0.182:11000",
      "label": "OPTIONS sip:10.26.0.182:11000 SIP/2.0",
      "parser": "10.26.0.182:9060",
      "raw": [
        "OPTIONS sip:10.26.0.182:11000 SIP/2.0",
        "Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK6551.e2149fb2000000000000000000000000.0",
        "To: <sip:10.26.0.182:11000>",
        "From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-4acf",
        "CSeq: 10 OPTIONS",
        "Call-ID: 5dca43e524c680cf-13867@10.26.0.182",
        "Max-Forwards: 70",
        "Content-Length: 0"
      ],
      "ref_timestamp": "63601204677.88179",
      "src": "10.26.0.182:5060",
      "timestamp": 63601204677.8817
      }

Find more example chunks in [the test suite](https://github.com/2600hz/kazoo/blob/master/applications/call_inspector/test/ci_chunk_tests.erl).

### Analysis

An **analysis** is the app's interpretation of a sequence of chunks.
It has business logic that can find and report a bad exchange.

*This business logic is not implemented yet.*


## Maintenance module

### Starting the `call_inspector` application

First ensure the `call_inspector` application is up and running:

    sup kapps_controller [re]start_app call_inspector

### Display a list of active parsers

List running log or HEP parsers:

    sup call_inspector_maintenance list_active_parsers

A parser is uniquely identified by its name:
* For log parsers, the UUID is the absolute path to the log file
* For HEP parsers it is the IP address and port of the node sending HEP packets: `'{IP}:{PORT}'`.

This way one is not able to parse the same input stream multiple times,
as it would result in storing duplicated data.
*However, the algorithm reordering chunks is able to remove those duplicates.*

### Starting and stopping parsers

When starting a parser its unique name is displayed.
If one starts a parser that is already running (identification done with the unique name),
then no new parser is created, the already running one is unharmed and its unique name is displayed.

#### Start an HEP packets parser

    sup call_inspector_maintenance start_hep_parser {IP} {PORT}

`{IP}` and `{PORT}` represent the IP address and port of the node receiving SIP packets and sending them to
the app as HEP packets.

Example:

    $ sup call_inspector_maintenance start_hep_parser 10.26.0.182 9060
    started '10.26.0.182:9060'
    $ sup call_inspector_maintenance list_active_parsers
    '10.26.0.182:9060'

##### Setup HEP packets capturing for FreeSWITCH

sofia.conf.xml

```xml
<param name="capture-server" value="udp:192.81.135.31:9060"/>
```

sipinterface_1.xml

```xml
<param name="capture-server" value="udp:192.81.135.31:9060"/>
<param name="sip-capture" value="yes"/>
```

In your fs_cli: `sofia global siptrace on`

##### Setup HEP packets capturing for Kamailio

Add this to your default.cfg

```
loadmodule "siptrace.so"
modparam("siptrace", "duplicate_uri", "sip:192.168.56.42:9061")
modparam("siptrace", "hep_mode_on", 1)
modparam("siptrace", "hep_version", 2)
modparam("siptrace", "hep_capture_id", 1337)
modparam("siptrace", "trace_to_database", 0)
modparam("siptrace", "trace_flag", 22)
modparam("siptrace", "trace_on", 1)
```

#### Start a Kamailio or FreeSWITCH logs parser

    sup call_inspector_maintenance start_kamailio_parser {LOGFILE} {IP} {PORT}
    sup call_inspector_maintenance start_freeswitch_parser {LOGFILE} {IP} {PORT}

`{IP}` & `{PORT}` are the Kamailio or FreeSWITCH IP address & port.
This information is used to fill up the `"src"` or `"dst"` fields of chunks as these fields would otherwise be incomplete.
`{LOGFILE}` is an absolute path to either a Kamailio or FreeSWITCH log file.

Example:

    $ sup call_inspector_maintenance start_kamailio_parser /var/log/kamailio/kamailio.log 10.26.0.12 5060
    started '/var/log/kamailio/kamailio.log'
    $ sup call_inspector_maintenance start_freeswitch_parser /var/log/freeswitch/debug.log 10.26.0.21 11000
    started '/var/log/freeswitch/debug.log'
    $ sup call_inspector_maintenance list_active_parsers
    '/var/log/kamailio/kamailio.log'
    '/var/log/freeswitch/debug.log'

Note that when a log parser is started the whole log file will get parsed.
While this should not take much resources, it can take a while before currently occurring calls are accessible through this app.

Likewise, if the **call_inspector** app crashes its parser processes are restarted.
This means that for log parsers the whole log file will get parsed again.

#### Stopping a parser

Any kind of parser can be stopped with this command:

    sup call_inspector_maintenance stop_active_parser {UUID}

`{UUID}` is the unique name of a parser.
Stopping a parser that is not started (or that does not exist) results in an error but does not crash the app.

Example:

    $ sup call_inspector_maintenance start_hep_parser 10.26.0.12 9060
    started '10.26.0.12:9060'
    $ sup call_inspector_maintenance start_hep_parser 10.26.0.21 9061
    started '10.26.0.21:9061'
    $ sup call_inspector_maintenance list_active_parsers
    '10.26.0.12:9060'
    '10.26.0.21:9061'
    $ sup call_inspector_maintenance stop_active_parser 10.26.0.21:9061
    ok
    $ sup call_inspector_maintenance list_active_parsers
    '10.26.0.12:9060'


### Fetch ordered chunks

To investigate a failing call one used to have to `grep` through different log files on multiple machines
then make sense of the skewed timestamps.

This application does all this for you and uses a clever algorithm to **reorder the dialogue** that makes up the multi-leg SIP exchange.

Dialogues are fetchable by **call-id** only:

    sup call_inspector_maintenance callid_details {CALLID}
    sup call_inspector_maintenance inspect_call_id {CALLID}

`{CALLID}` is the string that uniquely identifies the exchange.
It is the string that one would be using when `grep`ing logs.

Example:

    $ sup call_inspector_maintenance callid_details '5dca43e524c680cf-13867@10.26.0.182' | python -mjson.tool
    [
      {
        "call-id": "5dca43e524c680cf-13867@10.26.0.182",
        "dst": "10.26.0.182:11000",
        "label": "OPTIONS sip:10.26.0.182:11000 SIP/2.0",
        "parser": "10.26.0.182:9060",
        "raw": [
          "OPTIONS sip:10.26.0.182:11000 SIP/2.0",
          "Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK6551.e2149fb2000000000000000000000000.0",
          "To: <sip:10.26.0.182:11000>",
          "From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-4acf",
          "CSeq: 10 OPTIONS",
          "Call-ID: 5dca43e524c680cf-13867@10.26.0.182",
          "Max-Forwards: 70",
          "Content-Length: 0"
        ],
        "ref_timestamp": "63601204677.88179",
        "src": "10.26.0.182:5060",
        "timestamp": 63601204677.8817
      },
      {
        "call-id": "5dca43e524c680cf-13867@10.26.0.182",
        "dst": "10.26.0.182:5060",
        "label": "SIP/2.0 200 OK",
        "parser": "10.26.0.182:9060",
        "raw": [
          "SIP/2.0 200 OK",
          "Via: SIP/2.0/UDP 10.26.0.182;branch=z9hG4bK6551.e2149fb2000000000000000000000000.0",
          "From: <sip:sipcheck@10.26.0.182>;tag=6a9eb17cd14528bda74bd05e73f170de-4acf",
          "To: <sip:10.26.0.182:11000>;tag=15vvXS6m6868r",
          "Call-ID: 5dca43e524c680cf-13867@10.26.0.182",
          "CSeq: 10 OPTIONS",
          "Contact: <sip:10.26.0.182:11000>",
          "User-Agent: 2600Hz",
          "Accept: application/sdp",
          "Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, MESSAGE, INFO, UPDATE, REGISTER, REFER, NOTIFY, PUBLISH, SUBSCRIBE",
          "Supported: path, replaces",
          "Allow-Events: talk, hold, conference, presence, as-feature-event, dialog, line-seize, call-info, sla, include-session-description, presence.winfo, message-summary, refer",
          "Content-Length: 0"
        ],
        "ref_timestamp": "63601204677.892494",
        "src": "10.26.0.182:11000",
        "timestamp": 63601204677.89242
      }
    ]


### Flushing chunks

To remove one particular stored call-id:

    $ sup call_inspector_maintenance flush '5dca43e524c680cf-13867@10.26.0.182'
    ok
    $ sup call_inspector_maintenance callid_details '5dca43e524c680cf-13867@10.26.0.182'
    []

Flush everything:

    sup call_inspector_maintenance flush


## logrotate configuration

The app will keep filling up its storage directory unless some script removes the oldest ones regularly.

```
/var/log/kazoo/call_inspector/*/*/* {
    weekly
    missingok
    rotate 0
}
```

You can test this command with `logrotate -d`.
