%% Route used to publish new voicemail alerts
-define(RATING_REQ, <<"call.rating">>). %% Routing key to bind with in AMQP

%% AMQP fields for Rating Request
-define(RATING_REQ_HEADERS, [<<"To-DID">>, <<"From-DID">>, <<"Rate-Options">>, <<"Direction">>]).
-define(OPTIONAL_RATING_REQ_HEADERS, []).
-define(RATING_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"rating_req">>}
			    ,{<<"Direction">>, [<<"inbound">>, <<"outbound">>]}
			   ]).
-define(RATING_REQ_TYPES, []).

%% AMQP fields for Rating Response
-define(RATING_RESP_HEADERS, [<<"Rate">>, <<"Rate-Increment">>, <<"Rate-Minimum">>, <<"Surcharge">>, <<"Base-Cost">>]).
-define(OPTIONAL_RATING_RESP_HEADERS, [<<"Rate-Name">>]).
-define(RATING_RESP_VALUES, [{<<"Event-Category">>, <<"call">>}
			     ,{<<"Event-Name">>, <<"rating_resp">>}
			    ]).
-define(RATING_RESP_TYPES, []).
