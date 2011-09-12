%% Route used to publish new voicemail alerts
-define(NOTIFY_VOICEMAIL_NEW, <<"notification.voicemail.new">>).

-define(NEW_VOICEMAIL_HEADERS, [<<"From-User">>, <<"From-Realm">>, <<"To-User">>, <<"To-Realm">>
				    ,<<"Account-DB">>, <<"Voicemail-Box">>, <<"Voicemail-Name">>, <<"Voicemail-Timestamp">>]).
-define(OPTIONAL_NEW_VOICEMAIL_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Call-ID">>]).
-define(NEW_VOICEMAIL_VALUES, [{<<"Event-Category">>, <<"notification">>}
			       ,{<<"Event-Name">>, <<"new_voicemail">>}
			      ]).
-define(NEW_VOICEMAIL_TYPES, []).
