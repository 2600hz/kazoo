%% Route used to publish events related to crossbar documents
-define(DOCUMENT_EVENT, <<"document.event">>).

-define(DOCUMENT_EVENT_HEADERS, [<<"ID">>, <<"Rev">>, <<"Type">>, <<"Account-DB">>, <<"Account-ID">>,
				 <<"Custom-Fields">>]).
-define(OPTIONAL_DOCUMENT_EVENT_HEADERS, [<<"Date-Modified">>, <<"Date-Created">>, <<"Version">>,
					 <<"Event-Type">>, <<"Event-Category">>, <<"Event-Name">>]).
-define(DOCUMENT_EVENT_VALUES, [{<<"Event-Category">>, <<"crossbar">>}
			       ,{<<"Event-Name">>, <<"document_event">>}
			       ,{<<"Event-Type">>, [<<"created">>, <<"edited">>, <<"deleted">>]}
			      ]).
-define(DOCUMENT_EVENT_TYPES, []).
