-ifndef(OMNIPRESENCE_API_HRL).

-define(SUBSCRIPTIONS_EXCHANGE, <<"dialoginfo_subs">>).
-define(UPDATES_EXCHANGE, <<"dialoginfo">>).

-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, [<<"Queue">>]).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(wh_util:to_integer(V)) end}]).

-define(PRESENCE_STATES, [<<"trying">>, <<"early">>
                          ,<<"confirmed">>, <<"terminated">>
                         ]).

-define(UPDATE_HEADERS, [<<"To">>, <<"From">>, <<"State">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"From-Tag">>, <<"To-Tag">>, <<"Call-ID">>]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"update">>}
                        ,{<<"State">>, ?PRESENCE_STATES}
                       ]).
-define(UPDATE_TYPES, []).

-define(FLUSH_HEADERS, [<<"Type">>]).
-define(OPTIONAL_FLUSH_HEADERS, [<<"User">>]).
-define(FLUSH_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"flush">>}
                       ]).
-define(FLUSH_TYPES, []).

-define(KEY_SEARCH_REQ, <<"presence.search_req">>).

-define(SEARCH_REQ_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, [<<"Username">>]).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"search_req">>}
                          ]).
-define(SEARCH_REQ_TYPES, []).

-define(SEARCH_RESP_HEADERS, [<<"Subscriptions">>]).
-define(OPTIONAL_SEARCH_RESP_HEADERS, []).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                             ,{<<"Event-Name">>, <<"search_resp">>}]).
-define(SEARCH_RESP_TYPES, []).

-define(KEY_RESET, <<"presence.reset">>).

-define(RESET_HEADERS, [<<"Realm">>, <<"Username">>]).
-define(OPTIONAL_RESET_HEADERS, []).
-define(RESET_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"reset">>}
                      ]).
-define(RESET_TYPES, []).


-define(OMNIPRESENCE_API_HRL, 'true').
-endif.
