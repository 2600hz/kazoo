-define(EMPTYRESPONSE, "<document type=\"freeswitch/xml\"></document>").

%% not including dial string section so we can manually do the bridging
%% or something like that (ask Darren).
-define(REGISTERRESPONSE,
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

-define(REGISTER_NOPASS_RESPONSE,
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
