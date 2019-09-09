-ifndef(KZ_API_HEADERS).

-include_lib("kazoo/include/kz_api_literals.hrl").

-type api_formatter_return() :: {'ok', iolist()} | {'error', string()}.
-type api_headers() :: kz_term:ne_binaries() | [kz_term:ne_binary() | kz_term:ne_binaries()].

-type api_types() :: [{kz_term:ne_binary(), fun()}].
-type valid_value() :: kz_term:ne_binary() | integer().
-type api_valid_values() :: [{kz_term:ne_binary(), valid_value() | [valid_value()]}].

-record(kapi_definition, {name :: kz_term:ne_binary()
                         ,friendly_name :: kz_term:ne_binary()
                         ,description :: kz_term:ne_binary()
                         ,build_fun :: fun((kz_term:api_terms()) -> api_formatter_return())
                         ,validate_fun :: fun((kz_term:api_terms()) -> boolean())
                         ,publish_fun :: fun((...) -> 'ok')
                         ,binding = 'undefined' :: kz_term:api_ne_binary()
                         ,restrict_to = 'undefined' :: kz_term:api_atom()
                         ,required_headers :: api_headers()
                         ,optional_headers :: api_headers()
                         ,values :: api_valid_values()
                         ,types :: api_types()
                         }).

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
%%%
%%% eg: -define(FOO_TYPES, [{<<"baz">>, fun(V) -> lists:member(V, props:get_value(<<"baz">>, ?FOO_VALUES)) end}]).
%%%   would define a function to validate the value of key <<"baz">> in the same way ?FOO_VALUES does.
%%%
%%% All four macros must be defined; OPTIONAL, VALUES, and TYPES can be empty lists.

%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS
       ,[?KEY_APP_NAME
        ,?KEY_APP_VERSION
        ,?KEY_EVENT_CATEGORY
        ,?KEY_EVENT_NAME
        ,?KEY_MSG_ID
        ]).
-define(OPTIONAL_DEFAULT_HEADERS
       ,[?KEY_ACCESS_GROUP
        ,?KEY_DEFER_RESPONSE
        ,?KEY_DESTINATION_SERVER
        ,?KEY_GEO_LOCATION
        ,?KEY_LOG_ID
        ,?KEY_API_ACCOUNT_ID
        ,?KEY_API_CALL_ID
        ,?KEY_NODE
        ,?KEY_QUEUE_ID
        ,?KEY_RAW_HEADERS
        ,?KEY_SERVER_ID
        ,?KEY_REQUEST_FROM_PID
        ,?KEY_REPLY_TO_PID
        ,?KEY_AMQP_BROKER
        ,?KEY_AMQP_ZONE
        ]).
-define(DEFAULT_VALUES, [{?KEY_NODE, kz_term:to_binary(node())}
                        ,{?KEY_MSG_ID, kz_binary:rand_hex(16)}
                        ]).
-define(DEFAULT_TYPES, [{?KEY_SERVER_ID, fun is_binary/1}
                       ,{?KEY_EVENT_CATEGORY, fun is_binary/1}
                       ,{?KEY_EVENT_NAME, fun is_binary/1}
                       ,{?KEY_APP_NAME, fun is_binary/1}
                       ,{?KEY_APP_VERSION, fun is_binary/1}
                       ,{?KEY_RAW_HEADERS, fun is_binary/1}
                       ,{?KEY_DESTINATION_SERVER, fun is_binary/1}
                       ,{?KEY_GEO_LOCATION, fun is_binary/1}
                       ,{?KEY_ACCESS_GROUP, fun is_binary/1}
                       ,{?KEY_TENANT_ID, fun is_binary/1}
                       ,{?KEY_MSG_ID, fun is_binary/1}
                       ,{?KEY_AMQP_BROKER, fun is_binary/1}
                       ,{?KEY_AMQP_ZONE, fun is_binary/1}
                       ]).

%% Error Responses
-define(ERROR_RESP_HEADERS, [?KEY_ERROR_MESSAGE]).
-define(OPTIONAL_ERROR_RESP_HEADERS, [?KEY_REQUEST
                                     ,?KEY_API_CALL_ID
                                     ,<<"Custom-Channel-Vars">>
                                     ,<<"Disposition">>
                                     ,<<"Hangup-Cause">>
                                     ,<<"Hangup-Code">>
                                     ]).
-define(ERROR_RESP_VALUES, [{?KEY_EVENT_CATEGORY, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

-define(KZ_API_HEADERS, 'true').
-endif.
