-define(EMPTYRESPONSE, "<document type=\"freeswitch/xml\"></document>").

%% not including dial string section so we can manually do the bridging
%% or something like that (ask Darren).
-define(REGISTER_HASH_RESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"a1-hash\" value=\"~s\"/>
				</params>
			</user>
		</domain>
	</section>
</document>").

-define(REGISTER_PASS_RESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"password\" value=\"\"/>
				</params>
			</user>
		</domain>
	</section>
</document>").

-define(ROUTE_BRIDGE_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Bridge Response\">
    <context name=\"context_2\">
    ~s
    </context>
  </section>
</document>").

%% [IndexNum :: integer(), BypassMedia :: boolean
%% ,ChannelVars :: "{var=val}"
%% ,Route :: "sip:+12038293150@sip.flowroute.com"]
-define(ROUTE_BRIDGE_EXT,
"<extension name=\"match_~p\" continue=\"true\">
  <condition>
    <action application=\"set\" data=\"bypass_media=~s\"/>
    <action application=\"set\" data=\"hangup_after_bridge=true\"/>
    <action application=\"set\" data=\"continue_on_fail=CALL_REJECTED,NORMAL_TEMPORARY_FAILURE,INCOMPATIBLE_DESTINATION,NO_ROUTE_DESTINATION,INVALID_NUMBER_FORMAT,NO_USER_RESPONSE,USER_BUSY,NETWORK_OUT_OF_ORDER\"/>
    <action application=\"bridge\" data=\"~ssofia/sipinterface_1/~s\" />
  </condition>
</extension>").

-define(ROUTE_PARK_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Park Response\">
    <context name=\"context_2\">
      <extension name=\"park\">
        <condition>
          <action application=\"park\" />
        </condition>
      </extension>
    </context>
  </section>
</document>").

-define(ROUTE_ERROR_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"dialplan\" description=\"Route Bridge Response\">
    <context name=\"context_2\">
      <action application=\"respond\" data=\"~s~s\" />
    </context>
  </section>
</document>").

-define(ROUTE_NOT_FOUND_RESPONSE,
"<document type=\"freeswitch/xml\">
  <section name=\"result\">
    <result status=\"not found\" />
  </section>
</document>").
