-ifndef(KZ_API_HEADERS).

-include_lib("kazoo/include/kz_types.hrl").

-type api_formatter_return() :: {'ok', iolist()} | {'error', string()}.
-type api_headers() :: ne_binaries().

%%% *_HEADERS defines a list of Keys that must exist in every message of type *
%%% (substitute AUTHN_REQ, AUTHN_RESP, etc, for *) to be considered valid.
%%%
%%% OPTIONAL_*_HEADERS defines a list of Keys that will be included in the final
%%% message if included in the passed in Proplist.
%%%
%%% *_VALUES defines a proplist of {Key, Value} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Value is either a singular value or a list
%%% of values that the resulting message can have, given Key.
%%% If Value is not a list, a direct match is required to validate;
%%% if Value is a list of singular values, the set value must be a member of the Value list
%%% eg: -define(FOO_HEADERS, [<<"bar">>]).
%%%     -define(OPTIONAL_FOO_HEADERS, [<<"baz">>]).
%%%     -define(FOO_VALUES, [{<<"bar">>, <<"yes">>}, {<<"baz">>, [<<"abe">>, <<"bea">>, <<"eab">>]}]).
%%%     when foo_v(Prop) is called, Prop MUST contain key <<"bar">> with value <<"yes">>, and MAY
%%%     contain key <<"baz">>; if <<"baz">> exists, it can only have values <<"abe">>, <<"bea">>, or <<"eab">>.
%%%     foo_v([]) -> fails because key <<"bar">> is missing
%%%     foo_v([{<<"bar">>, <<"no">>}]) -> fails because <<"bar">> can only have value <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>}]) -> passes!
%%%     foo_v([{<<"baz">>, <<"abe">>}]) -> fails, no key <<"bar">>
%%%     foo_v([{<<"bar">>, <<"no">>, }, {<<"baz">>, <<"abe">>}]) -> fails, <<"bar">> can only be <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"zzz">>}]) -> fails, <<"zzz">> is not in ?FOO_VALUES
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"eab">>}]) -> passes!
%%%
%%% *_TYPES defines a proplist of {Key, Type} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Type defines a function that validates a passed in value
%%% is an appropriate type for the given Key, returning a boolean. If Key is not in the passed-in
%%% message, true is returned without running the Type fun.
%%% @spec Type :: function(Value :: any()) -> boolean()
%%%
%%% eg: -define(FOO_TYPES, [{<<"baz">>, fun(V) -> lists:member(V, proplists:get_value(<<"baz">>, ?FOO_VALUES)) end}]).
%%%   would define a function to validate the value of key <<"baz">> in the same way ?FOO_VALUES does.
%%%
%%% All four macros must be defined; OPTIONAL, VALUES, and TYPES can be empty lists.

-define(KEY_EVENT_CATEGORY, <<"Event-Category">>).
-define(KEY_EVENT_NAME, <<"Event-Name">>).
-define(KEY_APP_NAME, <<"App-Name">>).
-define(KEY_APP_VERSION, <<"App-Version">>).
-define(KEY_MSG_ID, <<"Msg-ID">>).
-define(KEY_MSG_REPLY_ID, <<"Msg-Reply-ID">>).
-define(KEY_NODE, <<"Node">>).
-define(KEY_SERVER_ID, <<"Server-ID">>).
-define(KEY_QUEUE_ID, <<"Queue-ID">>).
-define(KEY_LOG_ID, <<"System-Log-ID">>).

%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS, [?KEY_APP_NAME
                         ,?KEY_APP_VERSION
                         ,?KEY_EVENT_CATEGORY
                         ,?KEY_EVENT_NAME
                         ,?KEY_MSG_ID
                         ]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
                                  ,<<"Geo-Location">>, <<"Access-Group">>
                                  ,?KEY_NODE, ?KEY_SERVER_ID, ?KEY_QUEUE_ID
                                  ,<<"Defer-Response">>, ?KEY_LOG_ID
                                  ]).
-define(DEFAULT_VALUES, [{?KEY_NODE, kz_util:to_binary(node())}
                        ,{?KEY_MSG_ID, kz_util:rand_hex_binary(16)}
                        ]).
-define(DEFAULT_TYPES, [{?KEY_SERVER_ID, fun is_binary/1}
                       ,{?KEY_EVENT_CATEGORY, fun is_binary/1}
                       ,{?KEY_EVENT_NAME, fun is_binary/1}
                       ,{?KEY_APP_NAME, fun is_binary/1}
                       ,{?KEY_APP_VERSION, fun is_binary/1}
                       ,{<<"Raw-Headers">>, fun is_binary/1}
                       ,{<<"Destination-Server">>, fun is_binary/1}
                       ,{<<"Geo-Location">>, fun is_binary/1}
                       ,{<<"Access-Group">>, fun is_binary/1}
                       ,{<<"Tenant-ID">>, fun is_binary/1}
                       ,{?KEY_MSG_ID, fun is_binary/1}
                       ]).

%% Error Responses
-define(ERROR_RESP_HEADERS, [<<"Error-Message">>]).
-define(OPTIONAL_ERROR_RESP_HEADERS, [<<"Request">>, <<"Call-ID">>]).
-define(ERROR_RESP_VALUES, [{?KEY_EVENT_CATEGORY, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

-define(KZ_API_HEADERS, 'true').
-endif.
