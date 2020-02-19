-ifndef(KAPI_WEBSOCKETS_HRL).

-include_lib("kazoo/include/kz_api_literals.hrl").

-define(MODULE_REQ_ROUTING_KEY, <<"ws.module_req">>).
-define(MODULE_REQ_HEADERS, [<<"Module">>, <<"Action">>]).
-define(OPTIONAL_MODULE_REQ_HEADERS, [<<"Persist">>]).
-define(MODULE_REQ_VALUES, [{<<"Event-Category">>,<<"websockets">>}
                           ,{<<"Event-Name">>, <<"module_req">>}
                           ,{<<"Action">>, [<<"start">>, <<"stop">>]}
                           ]).
-define(MODULE_REQ_TYPES, [{<<"Persist">>, fun kz_term:is_boolean/1}
                          ,{<<"Module">>, fun is_binary/1}
                          ]).

-define(MODULE_RESP_HEADERS, [<<"Persisted">>, <<"Started">>]).
-define(OPTIONAL_MODULE_RESP_HEADERS, [<<"Error">>]).
-define(MODULE_RESP_VALUES, [{<<"Event-Category">>,<<"websockets">>}
                            ,{<<"Event-Name">>, <<"module_resp">>}
                            ]).
-define(MODULE_RESP_TYPES, [{<<"Persisted">>, fun kz_term:is_boolean/1}
                           ,{<<"Started">>, fun kz_term:is_boolean/1}
                           ,{<<"Error">>, fun is_binary/1}
                           ]).

-define(WEBSOCKETS_VALUES, [{?KEY_EVENT_CATEGORY, <<"websockets">>}]).

%% websockets whapp routing keys for responses to clients
-define(KEY_WEBSOCKETS_GET_REQ, <<"websockets.get">>).

%% Configuration Document Update
%% request to read
-define(WEBSOCKETS_GET_REQ_HEADERS, [?KEY_API_ACCOUNT_ID]).
-define(OPTIONAL_WEBSOCKETS_GET_REQ_HEADERS, [<<"Auth-Account-ID">>, <<"Socket-ID">>]).
-define(WEBSOCKETS_GET_REQ_VALUES, [{?KEY_EVENT_NAME, <<"get_req">>}
                                    | ?WEBSOCKETS_VALUES
                                   ]).

%% answer to a read request
-define(WEBSOCKETS_GET_RESP_HEADERS, []).
-define(OPTIONAL_WEBSOCKETS_GET_RESP_HEADERS, [<<"Data">>]).
-define(WEBSOCKETS_GET_RESP_VALUES, [{?KEY_EVENT_NAME, <<"get_resp">>}
                                     | ?WEBSOCKETS_VALUES
                                    ]).

-define(WEBSOCKETS_TYPES, []).

-define(KAPI_WEBSOCKETS_HRL, 'true').
-endif.
