-ifndef(KAPI_BLACKHOLE_HRL).

-include_lib("kazoo/include/kz_api_literals.hrl").

-define(MODULE_REQ_ROUTING_KEY, <<"bh.module_req">>).
-define(MODULE_REQ_HEADERS, [<<"Module">>, <<"Action">>]).
-define(OPTIONAL_MODULE_REQ_HEADERS, [<<"Persist">>]).
-define(MODULE_REQ_VALUES, [{<<"Event-Category">>,<<"blackhole">>}
                           ,{<<"Event-Name">>, <<"module_req">>}
                           ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                           ]).
-define(MODULE_REQ_TYPES, [{<<"Persist">>, fun kz_term:is_boolean/1}
                          ,{<<"Module">>, fun is_binary/1}
                          ]).

-define(MODULE_RESP_HEADERS, [<<"Persisted">>, <<"Started">>]).
-define(OPTIONAL_MODULE_RESP_HEADERS, [<<"Error">>]).
-define(MODULE_RESP_VALUES, [{<<"Event-Category">>,<<"blackhole">>}
                            ,{<<"Event-Name">>, <<"module_resp">>}
                            ]).
-define(MODULE_RESP_TYPES, [{<<"Persisted">>, fun kz_term:is_boolean/1}
                           ,{<<"Started">>, fun kz_term:is_boolean/1}
                           ,{<<"Error">>, fun is_binary/1}
                           ]).

-define(BLACKHOLE_VALUES, [{?KEY_EVENT_CATEGORY, <<"blackhole">>}]).


%% blackhole whapp routing keys for responses to clients
-define(KEY_BLACKHOLE_GET_REQ, <<"blackhole.get">>).

%% Configuration Document Update
%% request to read
-define(BLACKHOLE_GET_REQ_HEADERS, []).
-define(OPTIONAL_BLACKHOLE_GET_REQ_HEADERS, [?KEY_API_ACCOUNT_ID, <<"Socket-ID">>]).
-define(BLACKHOLE_GET_REQ_VALUES, [{?KEY_EVENT_NAME, <<"get_req">>}
                                   | ?BLACKHOLE_VALUES
                                  ]).

%% answer to a read request
-define(BLACKHOLE_GET_RESP_HEADERS, []).
-define(OPTIONAL_BLACKHOLE_GET_RESP_HEADERS, [<<"Data">>]).
-define(BLACKHOLE_GET_RESP_VALUES, [{?KEY_EVENT_NAME, <<"get_resp">>}
                                    | ?BLACKHOLE_VALUES
                                   ]).

-define(BLACKHOLE_TYPES, []).


-define(KAPI_BLACKHOLE_HRL, 'true').
-endif.
