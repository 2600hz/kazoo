-ifndef(FREESWITCH_XML_HRL).
-define(EMPTYRESPONSE, ""). %"<document type=\"freeswitch/xml\"></document>").

%% not including dial string section so we can manually do the bridging
%% or something like that (ask Darren).
%% [Domain, User, Hash, ChannelVars]
-define(REGISTER_HASH_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"directory\">
    <domain name=\"~s\">
      <user id=\"~s\">
        <params>
          <param name=\"a1-hash\" value=\"~s\"/>
        </params>
        <variables>
          ~s
        </variables>
      </user>
    </domain>
  </section>
</document>").
%% [Domain, User, Password, ChannelVars]
-define(REGISTER_PASS_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"directory\">
    <domain name=\"~s\">
      <user id=\"~s\">
        <params>
          <param name=\"password\" value=\"~s\"/>
        </params>
        <variables>
          ~s
        </variables>
      </user>
    </domain>
  </section>
</document>").

%% create a list of these params to inject into the REGISTER_*_RESPONSE
-define(REGISTER_CHANNEL_PARAM, "          <variable name=\"~s\" value=\"~s\" />~n").

-define(ROUTE_BRIDGE_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Bridge Response\">
    <context name=\"~s\">
    ~s
      <extension name=\"failed_bridge\" continue=\"false\">
        <condition>
          <action application=\"respond\" data=\"${bridge_hangup_cause}\" />
        </condition>
      </extension>
    </context>
  </section>
</document>").

%% [IndexNum :: integer(), BypassMedia :: boolean
%% ,ChannelVars :: "[var=val]"
%% ,Route :: "sip:+12038293150@sip.flowroute.com"]
-define(ROUTE_BRIDGE_EXT,
"<extension name=\"match_~p\" continue=\"true\">
  <condition>
    <action application=\"set\" data=\"bypass_media=~s\"/>
    <action application=\"set\" data=\"hangup_after_bridge=true\"/>
    <action application=\"set\" data=\"failure_causes=NORMAL_CLEARING,ORIGINATOR_CANCEL,CRASH\"/>
    <action application=\"bridge\" data=\"~s~s\" />
  </condition>
</extension>").

-define(ROUTE_PARK_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Park Response\">
    <context name=\"~s\">
      <extension name=\"park\">
        <condition>
          <action application=\"park\" />
        </condition>
      </extension>
    </context>
  </section>
</document>").

%% When a registration lookup fails, send a 503 or some other soft-error
%% IndexNum :: integer()
%% ErrorCode :: integer()
-define(ROUTE_BRIDGE_ERROR,
"<extension name=\"match_~p\" continue=\"true\">
  <condition>
    <action application=\"respond\" data=\"~p\" />
  </condition>
</extension>").

-define(ROUTE_ERROR_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Bridge Response\">
    <context name=\"~s\">
      <extension>
        <condition>
          <action application=\"respond\" data=\"~s~s\" />
        </condition>
      </extension>
    </context>
  </section>
</document>").

-define(ROUTE_NOT_FOUND_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"result\">
    <result status=\"not found\" />
  </section>
</document>").

-define(FS_ACL,
"<document type=\"freeswitch/xml\">
  <section name=\"configuration\">
    <configuration name=\"acls.conf\" description=\"Configuration data\">
      <network-lists>
        <list name=\"trusted\" default=\"deny\">
          <node type=\"allow\" cidr=\"12.23.34.45\"/>
          <node type=\"allow\" cird=\"43.32.12.54\"/>
        </list>
      </network-lists>
    </configuration>
  </section>
</document>").

-define(FS_CONFIG,
"<document type=\"freeswitch/xml\">
  <section name=\"configuration\">
    <configuration name=\"~s.conf\" description=\"Configuration data\">
      <settings>
      ~s
      </settings>
    </configuration>
  </section>
</document>").

-define(CONFIG_PARAM, "          <variable name=\"~s\" value=\"~s\" />~n").


-define(FREESWITCH_XML_HRL, true).
-endif.
