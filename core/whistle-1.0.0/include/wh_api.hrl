-ifndef(WH_API_HEADERS).

-ifndef(WHISTLE_TYPES_INCLUDED).
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-endif.

-type api_formatter_return() :: {'ok', iolist()} | {'error', string()}.
-type api_headers() :: [ne_binary(),...] | [].

%% For dialplan messages, what does the Invite-Format param accept as values?
-define(INVITE_FORMAT_TUPLE, {<<"Invite-Format">>, [<<"username">>, <<"e164">>, <<"npan">>, <<"1npan">>, <<"route">>]}).

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

%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS, [<<"Event-Category">>, <<"Event-Name">>
                              ,<<"App-Name">>, <<"App-Version">>
                              ,<<"Msg-ID">>
                         ]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
                                       ,<<"Geo-Location">>, <<"Access-Group">>
                                       ,<<"Tenant-ID">>, <<"Node">>, <<"Server-ID">>
                                       ,<<"Defer-Response">>
                                  ]).
-define(DEFAULT_VALUES, [{<<"Node">>, wh_util:to_binary(node())}
                         ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
                        ]).
-define(DEFAULT_TYPES, [{<<"Server-ID">>, fun is_binary/1}
                        ,{<<"Event-Category">>, fun is_binary/1}
                        ,{<<"Event-Name">>, fun is_binary/1}
                        ,{<<"App-Name">>, fun is_binary/1}
                        ,{<<"App-Version">>, fun is_binary/1}
                        ,{<<"Raw-Headers">>, fun is_binary/1}
                        ,{<<"Destination-Server">>, fun is_binary/1}
                        ,{<<"Geo-Location">>, fun is_binary/1}
                        ,{<<"Access-Group">>, fun is_binary/1}
                        ,{<<"Tenant-ID">>, fun is_binary/1}
                        ,{<<"Msg-ID">>, fun is_binary/1}
                       ]).

%% Error Responses
-define(ERROR_RESP_HEADERS, [<<"Error-Message">>]).
-define(OPTIONAL_ERROR_RESP_HEADERS, [<<"Request">>, <<"Call-ID">>]).
-define(ERROR_RESP_VALUES, [{<<"Event-Category">>, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

-define(WH_API_HEADERS, true).
-endif.
