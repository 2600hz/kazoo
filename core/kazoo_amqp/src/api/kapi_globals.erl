%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Globals API.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_globals).

-compile({'no_auto_import',[unregister/1]}).

-export([api_definitions/0, api_definition/1]).

-export([name/1]).
-export([message/1, reply/1, reason/1]).
-export([encode/1, encode_req/1, decode/1]).
-export([state/1]).
-export([is_pending/1
        ,is_none/1
        ]).
-export([timestamp/1]).
-export([node/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([query/1
        ,query_v/1
        ,publish_query/1
        ,publish_query/2
        ]).
-export([query_resp/1
        ,query_resp_v/1
        ,publish_query_resp/2
        ,publish_query_resp/3
        ]).
-export([send/1
        ,send_v/1
        ,publish_send/1
        ,publish_send/2
        ]).
-export([call/1
        ,call_v/1
        ,publish_call/1
        ,publish_call/2
        ]).
-export([reply_msg/1
        ,reply_msg_v/1
        ,publish_reply_msg/2
        ,publish_reply_msg/3
        ]).
-export([register/1
        ,register_v/1
        ,publish_register/1
        ,publish_register/2
        ]).
-export([register_resp/1
        ,register_resp_v/1
        ,publish_register_resp/2
        ,publish_register_resp/3
        ]).
-export([unregister/1
        ,unregister_v/1
        ,publish_unregister/1
        ,publish_unregister/2
        ]).

-export([publish_targeted_send/2, publish_targeted_send/3]).
-export([publish_targeted_call/2, publish_targeted_call/3]).

-include_lib("kz_amqp_util.hrl").

-define(GLOBALS_EXCHANGE, <<"globals">>).
-define(GLOBALS_EXCHANGE_TYPE, <<"topic">>).

%% Types & Accessors

-type state() :: 'none' | 'local' | 'pending' | 'remote' | 'registered'.
-export_type([state/0]).

-ifdef(TEST).
-export([routing_key/2
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [query_definition()
    ,query_resp_definition()
    ,send_definition()
    ,call_definition()
    ,reply_msg_definition()
    ,register_definition()
    ,register_resp_definition()
    ,unregister_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"query">>) ->
    query_definition();
api_definition(<<"query_resp">>) ->
    query_resp_definition();
api_definition(<<"send">>) ->
    send_definition();
api_definition(<<"call">>) ->
    call_definition();
api_definition(<<"reply_msg">>) ->
    reply_msg_definition();
api_definition(<<"register">>) ->
    register_definition();
api_definition(<<"register_resp">>) ->
    register_resp_definition();
api_definition(<<"unregister">>) ->
    unregister_definition().

-spec query_definition() -> kapi_definition:api().
query_definition() ->
    EventName = <<"query">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Query Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query/1}
              ,{fun kapi_definition:set_binding/2, fun routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec query_resp_definition() -> kapi_definition:api().
query_resp_definition() ->
    EventName = <<"query_resp">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Query Response">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Query Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun query_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun query_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_query_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"State">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Timestamp">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec send_definition() -> kapi_definition:api().
send_definition() ->
    EventName = <<"send">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Send Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Send Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun send/1}
              ,{fun kapi_definition:set_validate_fun/2, fun send_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_send/1}
              ,{fun kapi_definition:set_binding/2, fun routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Message">>
                                                            ,<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec call_definition() -> kapi_definition:api().
call_definition() ->
    EventName = <<"call">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Call Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Call Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun call/1}
              ,{fun kapi_definition:set_validate_fun/2, fun call_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_call/1}
              ,{fun kapi_definition:set_binding/2, fun routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Message">>
                                                            ,<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec reply_msg_definition() -> kapi_definition:api().
reply_msg_definition() ->
    EventName = <<"reply">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Reply Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Reply Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reply_msg/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reply_msg_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reply_msg/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Reply">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec register_definition() -> kapi_definition:api().
register_definition() ->
    EventName = <<"register">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Register Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Register Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun register/1}
              ,{fun kapi_definition:set_validate_fun/2, fun register_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_register/1}
              ,{fun kapi_definition:set_binding/2, fun routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"State">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Timestamp">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec register_resp_definition() -> kapi_definition:api().
register_resp_definition() ->
    EventName = <<"register_resp">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Register Response">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Register Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun register_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun register_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_register_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"State">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Timestamp">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec unregister_definition() -> kapi_definition:api().
unregister_definition() ->
    EventName = <<"unregister">>,
    Category = <<"globals">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Unregister Request">>}
              ,{fun kapi_definition:set_description/2, <<"Globals Unregister Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun unregister/1}
              ,{fun kapi_definition:set_validate_fun/2, fun unregister_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_unregister/1}
              ,{fun kapi_definition:set_binding/2, fun routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Name">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Reason">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec encode(any()) -> kz_term:ne_binary().
encode(Term) ->
    base64:encode(term_to_binary(Term)).

-spec decode(kz_term:api_binary()) -> any().
decode('undefined') -> 'undefined';
decode(Bin) ->
    binary_to_term(base64:decode(Bin)).

-spec maybe_encode(any()) -> any().
maybe_encode(<<131, _/binary>>=Encoded) ->
    base64:encode(Encoded);
maybe_encode(Term) ->
    encode(Term).

-spec maybe_decode(any()) -> any().
maybe_decode(<<131, _/binary>>=Encoded) ->
    binary_to_term(Encoded);
maybe_decode(MaybeEncoded) ->
    try base64:decode(MaybeEncoded) of
        Decoded ->
            binary_to_term(Decoded)
    catch
        'error':'function_clause' -> MaybeEncoded;
        'error':{'badmatch','false'} -> MaybeEncoded;
        'error':{'badarg', _} -> MaybeEncoded
    end.

-spec encode_req(kz_json:object() | kz_term:proplist()) -> any().
encode_req(Req) ->
    set_name(Req, name(Req)).

-spec name(kz_json:object() | kz_term:proplist()) -> any().
name(Props)
  when is_list(Props) ->
    maybe_decode(props:get_value(<<"Name">>, Props));
name(JObj) ->
    maybe_decode(kz_json:get_value(<<"Name">>, JObj)).

-spec set_name(kz_term:api_terms(), any()) -> kz_term:api_terms().
set_name(Req, 'undefined') -> Req;
set_name(Props, Name)
  when is_list(Props) ->
    props:set_value(<<"Name">>, maybe_encode(Name), Props);
set_name(JObj, Name) ->
    kz_json:set_value(<<"Name">>, maybe_encode(Name), JObj).

-spec message(kz_json:object()) -> kz_term:ne_binary().
message(JObj) ->
    maybe_decode(kz_json:get_value(<<"Message">>, JObj)).

-spec reply(kz_json:object()) -> kz_term:ne_binary().
reply(JObj) ->
    maybe_decode(kz_json:get_value(<<"Reply">>, JObj)).

-spec state(kz_json:object()) -> state().
state(JObj) ->
    kz_json:get_atom_value(<<"State">>, JObj).

-spec reason(kz_json:object()) -> any().
reason(JObj) ->
    maybe_decode(kz_json:get_value(<<"Reason">>, JObj)).

-spec is_pending(kz_json:object()) -> boolean().
is_pending(JObj) ->
    state(JObj) =:= 'pending'.

-spec is_none(kz_json:object()) -> boolean().
is_none(JObj) ->
    state(JObj) =:= 'none'.

-spec timestamp(kz_json:object()) -> integer().
timestamp(JObj) ->
    kz_json:get_integer_value(<<"Timestamp">>, JObj).

-spec node(kz_json:object()) -> atom().
node(JObj) ->
    kz_term:to_atom(kz_api:node(JObj), 'true').

%%------------------------------------------------------------------------------
%% @doc Query Request
%% @end
%%------------------------------------------------------------------------------
-spec query(kz_term:api_terms()) -> kz_api:api_formatter_return().
query(Req) ->
    kapi_definition:build_message(Req, query_definition()).

-spec query_v(kz_term:api_terms()) -> boolean().
query_v(Req) ->
    kapi_definition:validate(Req, query_definition()).

-spec publish_query(kz_term:api_terms()) -> 'ok'.
publish_query(JObj) ->
    publish_query(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query(Req, ContentType) ->
    Definition = query_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    publish((kapi_definition:binding(Definition))(<<"query">>, name(Req))
           ,Payload
           ,ContentType
           ).

%%------------------------------------------------------------------------------
%% @doc Query Response
%% @end
%%------------------------------------------------------------------------------
-spec query_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
query_resp(Req) ->
    kapi_definition:build_message(Req, query_resp_definition()).

-spec query_resp_v(kz_term:api_terms()) -> boolean().
query_resp_v(Req) ->
    kapi_definition:validate(Req, query_resp_definition()).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_query_resp(ServerId, JObj) ->
    publish_query_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_query_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_query_resp(ServerId, Req, ContentType) ->
    Definition = query_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Send Request
%% @end
%%------------------------------------------------------------------------------
-spec send(kz_term:api_terms()) -> kz_api:api_formatter_return().
send(Req) ->
    kapi_definition:build_message(Req, send_definition()).

-spec send_v(kz_term:api_terms()) -> boolean().
send_v(Req) ->
    kapi_definition:validate(Req, send_definition()).

-spec publish_send(kz_term:api_terms()) -> 'ok'.
publish_send(JObj) ->
    publish_send(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_send(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_send(Req, ContentType) ->
    Definition = send_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    publish((kapi_definition:binding(Definition))(<<"send">>, name(Req))
           ,Payload
           ,ContentType
           ).

-spec publish_targeted_send(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_send(ServerId, JObj) ->
    publish_targeted_send(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_send(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_send(ServerId, Req, ContentType) ->
    Definition = send_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Call Request
%% @end
%%------------------------------------------------------------------------------
-spec call(kz_term:api_terms()) -> kz_api:api_formatter_return().
call(Req) ->
    kapi_definition:build_message(Req, call_definition()).

-spec call_v(kz_term:api_terms()) -> boolean().
call_v(Req) ->
    kapi_definition:validate(Req, call_definition()).

-spec publish_call(kz_term:api_terms()) -> 'ok'.
publish_call(JObj) ->
    publish_call(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_call(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_call(Req, ContentType) ->
    Definition = call_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    publish((kapi_definition:binding(Definition))(<<"call">>, name(Req))
           ,Payload
           ,ContentType
           ).

-spec publish_targeted_call(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_targeted_call(ServerId, JObj) ->
    publish_targeted_call(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_call(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_call(ServerId, Req, ContentType) ->
    Definition = call_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Reply Request
%% @end
%%------------------------------------------------------------------------------
-spec reply_msg(kz_term:api_terms()) -> kz_api:api_formatter_return().
reply_msg(Req) ->
    kapi_definition:build_message(Req, reply_msg_definition()).

-spec reply_msg_v(kz_term:api_terms()) -> boolean().
reply_msg_v(Req) ->
    kapi_definition:validate(Req, reply_msg_definition()).

-spec publish_reply_msg(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_reply_msg(ServerId, JObj) ->
    publish_reply_msg(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_reply_msg(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_reply_msg(ServerId, Req, ContentType) ->
    Definition = reply_msg_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Register Request
%% @end
%%------------------------------------------------------------------------------
-spec register(kz_term:api_terms()) -> kz_api:api_formatter_return().
register(Req) ->
    kapi_definition:build_message(Req, register_definition()).

-spec register_v(kz_term:api_terms()) -> boolean().
register_v(Req) ->
    kapi_definition:validate(Req, register_definition()).

-spec publish_register(kz_term:api_terms()) -> 'ok'.
publish_register(Req) ->
    publish_register(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_register(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register(Req, ContentType) ->
    Definition = register_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    publish((kapi_definition:binding(Definition))(<<"register">>, name(Req))
           ,Payload
           ,ContentType
           ).

%%------------------------------------------------------------------------------
%% @doc Register Response
%% @end
%%------------------------------------------------------------------------------
-spec register_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
register_resp(Req) ->
    kapi_definition:build_message(Req, register_resp_definition()).

-spec register_resp_v(kz_term:api_terms()) -> boolean().
register_resp_v(Req) ->
    kapi_definition:validate(Req, register_resp_definition()).

-spec publish_register_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_register_resp(ServerId, JObj) ->
    publish_register_resp(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_register_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register_resp(ServerId, Req, ContentType) ->
    Definition = register_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Unregister Request
%% @end
%%------------------------------------------------------------------------------
-spec unregister(kz_term:api_terms()) -> kz_api:api_formatter_return().
unregister(Req) ->
    kapi_definition:build_message(Req, unregister_definition()).

-spec unregister_v(kz_term:api_terms()) -> boolean().
unregister_v(Req) ->
    kapi_definition:validate(Req, unregister_definition()).

-spec publish_unregister(kz_term:api_terms()) -> 'ok'.
publish_unregister(Req) ->
    publish_unregister(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_unregister(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unregister(Req, ContentType) ->
    Definition = unregister_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(encode_req(Req)
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    publish((kapi_definition:binding(Definition))(<<"unregister">>, name(Req))
           ,Payload
           ,ContentType
           ).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:new_exchange(?GLOBALS_EXCHANGE, ?GLOBALS_EXCHANGE_TYPE).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, Name).

bind_q(Q, [Event|T], Name) ->
    _ = kz_amqp_util:bind_q_to_exchange(Q
                                       ,(kapi_definition:binding(query_definition()))(Event, Name)
                                       ,?GLOBALS_EXCHANGE
                                       ),
    bind_q(Q, T, Name);
bind_q(_Q, [], _Name) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), any()) -> 'ok'.
unbind_q(Queue, Props) ->
    Name = props:get_value('name', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, Name).

unbind_q(Q, [Event|T], Name) ->
    _ = kz_amqp_util:unbind_q_from_exchange(Q
                                           ,(kapi_definition:binding(query_definition()))(Event, Name)
                                           ,?GLOBALS_EXCHANGE
                                           ),
    unbind_q(Q, T, Name);
unbind_q(_Q, [], _Name) -> 'ok'.

publish(Routing, Payload, ContentType) ->
    kz_amqp_util:basic_publish(?GLOBALS_EXCHANGE, Routing, Payload, ContentType).

-spec routing_key(kz_term:text(), any()) -> kz_term:ne_binary().
routing_key(Event, Name) when is_binary(Name) ->
    list_to_binary(["globals."
                   ,kz_term:to_binary(Event)
                   ,"."
                   ,kz_amqp_util:encode(Name)
                   ]);
routing_key(Event, Name) ->
    list_to_binary(["globals."
                   ,kz_term:to_binary(Event)
                   ,"."
                   ,kz_term:to_hex_binary(maybe_encode(Name))
                   ]).
