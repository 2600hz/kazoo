-ifndef(OMNIPRESENCE_API_HRL).

-define(SUBSCRIPTIONS_EXCHANGE, <<"dialoginfo_subs">>).
-define(UPDATES_EXCHANGE, <<"dialoginfo">>).

-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>, <<"Queue">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, []).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(wh_util:to_integer(V)) end}]).

-define(UPDATE_HEADERS, [<<"To">>, <<"From">>, <<"State">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"From-Tag">>, <<"To-Tag">>, <<"Call-ID">>]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"update">>}
                        ,{<<"State">>, [<<"trying">>, <<"early">>
                                        ,<<"confirmed">>, <<"terminated">>
                                       ]}
                       ]).
-define(UPDATE_TYPES, []).

-define(OMNIPRESENCE_API_HRL, 'true').
-endif.
