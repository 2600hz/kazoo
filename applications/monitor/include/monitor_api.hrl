%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Msg-ID">>, <<"Event-Category">>, <<"Event-Name">>
                  , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, []).
-define(DEFAULT_VALUES, [{<<"App-Name">>, <<"monitor">>}
            ,{<<"App-Version">>, <<"0.1.0">>}
            ]).
-define(DEFAULT_TYPES, [{<<"Server-ID">>, fun is_binary/1} 
            ,{<<"Event-Category">>, fun is_binary/1}
            ,{<<"Event-Category">>, fun is_binary/1}
            ,{<<"Event-Name">>, fun is_binary/1}
            ,{<<"App-Name">>, fun is_binary/1}
            ,{<<"App-Version">>, fun is_binary/1}
            ]).

%% Monitor Ping Request
-define(PING_REQ_HEADERS, [<<"Destination">>]).
-define(OPTIONAL_PING_REQ_HEADERS, [<<"Count">>]).
-define(PING_REQ_VALUES, [{<<"Event-Category">>, <<"task">>}
              ,{<<"Event-Name">>, <<"ping_req">>}
             ]).
-define(PING_REQ_TYPES, [{<<"Destination">>, fun is_binary/1}
             ,{<<"Count">>, fun is_binary/1}
            ]).

%% Monitor Ping Respons
-define(PING_RESP_HEADERS, [<<"Success">>]).
-define(OPTIONAL_PING_RESP_HEADERS, []).
-define(PING_RESP_VALUES, [{<<"Event-Category">>, <<"task">>}
              ,{<<"Event-Name">>, <<"ping_req">>}
              ,{<<"Success">>, [<<"true">>, <<"false">>]}
             ]).
-define(PING_RESP_TYPES, [{<<"Success">>, fun is_binary/1}]).


-type proplist() :: list(tuple(binary(), (binary() | list() | fun()) )).

