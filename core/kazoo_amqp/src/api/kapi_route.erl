%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Routing requests, responses, and wins!
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_route).

-export([api_definitions/0, api_definition/1]).

-export([req/1
        ,req_v/1
        ,publish_req/1
        ,publish_req/2
        ]).
-export([resp/1
        ,resp_v/1
        ,publish_resp/2
        ,publish_resp/3
        ]).
-export([win/1
        ,win_v/1
        ,publish_win/2
        ,publish_win/3
        ]).

-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ,get_auth_realm/1
        ,get_auth_user/1
        ,req_event_type/0
        ,is_actionable_resp/1

        ,account_id/1
        ,call_id/1
        ,control_queue/1
        ,fetch_id/1
        ]).

-include_lib("kz_amqp_util.hrl").
-include("kapi_dialplan.hrl").
-include("kapi_route.hrl").

-type req() :: kz_json:object().
-type resp() :: kz_json:object().

-export_type([req/0, resp/0]).

-ifdef(TEST).
-export([get_route_req_account_routing/2
        ,get_route_req_realm_routing/3
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()
    ,resp_definition()
    ,win_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"req">>) ->
    req_definition();
api_definition(<<"resp">>) ->
    resp_definition();
api_definition(<<"win">>) ->
    win_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    EventName = <<"route_req">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Dialplan Route Request">>}
              ,{fun kapi_definition:set_description/2, <<"Dialplan Route Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/1}
              ,{fun kapi_definition:set_binding/2, fun get_route_req_routing/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"From">>
                                                            ,<<"Request">>
                                                            ,<<"To">>
                                                            ,?KEY_CALL_ID
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Body">>
                                                            ,<<"Call-Direction">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Codecs">>
                                                            ,<<"Cost-Parameters">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"Custom-Routing-Headers">>
                                                            ,<<"Custom-SIP-Headers">>
                                                            ,<<"Context">>
                                                            ,<<"From-Network-Addr">>
                                                            ,<<"From-Network-Port">>
                                                            ,<<"From-Tag">>
                                                            ,<<"Geo-Location">>
                                                            ,<<"Max-Call-Length">>
                                                            ,<<"Media">>
                                                            ,<<"Message-ID">>
                                                            ,<<"Orig-IP">>
                                                            ,<<"Orig-Port">>
                                                            ,<<"Origination-Call-ID">>
                                                            ,<<"Prepend-CID-Name">>
                                                            ,<<"Resource-Type">>
                                                            ,<<"Ringback-Media">>
                                                            ,<<"SIP-Request-Host">>
                                                            ,<<"Switch-Hostname">>
                                                            ,<<"Switch-Nodename">>
                                                            ,<<"Switch-URI">>
                                                            ,<<"Switch-URL">>
                                                            ,<<"To-Tag">>
                                                            ,<<"Transcode">>
                                                            ,<<"Transfer-Media">>
                                                            ,<<"User-Agent">>
                                                            ,<<"Destination-Number">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Resource-Type">>, [<<"mms">>
                                       ,<<"sms">>
                                       ,<<"audio">>
                                       ,<<"video">>
                                       ,<<"chat">>
                                       ,<<"metaflow">>
                                       ]}
                ,{<<"Media">>, [<<"process">>, <<"proxy">>, <<"bypass">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Caller-ID-Name">>, fun erlang:is_binary/1}
                ,{<<"Caller-ID-Number">>, fun erlang:is_binary/1}
                ,{<<"Cost-Parameters">>, fun has_cost_parameters/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Event-Queue">>, fun erlang:is_binary/1}
                ,{<<"From">>, fun erlang:is_binary/1}
                ,{<<"Request">>, fun erlang:is_binary/1}
                ,{<<"To">>, fun erlang:is_binary/1}
                ,{?KEY_CALL_ID, fun erlang:is_binary/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec resp_definition() -> kapi_definition:api().
resp_definition() ->
    EventName = <<"route_resp">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Dialplan Route Response">>}
              ,{fun kapi_definition:set_description/2, <<"Dialplan Route Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Method">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Application-Data">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-URI">>
                                                            ,<<"From-User">>
                                                            ,<<"Plan-Data">>
                                                            ,<<"Pre-Park">>
                                                            ,<<"Ringback-Media">>
                                                            ,<<"Route-Error-Code">>
                                                            ,<<"Route-Error-Message">>
                                                            ,<<"Routes">>
                                                            ,<<"Transfer-Media">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Method">>, [<<"bridge">>
                                ,<<"park">>
                                ,<<"error">>
                                ,<<"sms">>
                                ,<<"plan">>
                                ,<<"application">>
                                ]
                 }
                ,{<<"Pre-Park">>, [<<"none">>, <<"ring_ready">>, <<"answer">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Route-Error-Code">>, fun erlang:is_binary/1}
                ,{<<"Route-Error-Message">>, fun erlang:is_binary/1}
                ,{<<"Routes">>, fun erlang:is_list/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec win_definition() -> kapi_definition:api().
win_definition() ->
    EventName = <<"route_win">>,
    Category = ?EVENT_CATEGORY,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Dialplan Route Winner">>}
              ,{fun kapi_definition:set_description/2, <<"Dialplan Route Winner">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun win/1}
              ,{fun kapi_definition:set_validate_fun/2, fun win_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_win/2}
              ,{fun kapi_definition:set_binding/2, 'undefined'}
              ,{fun kapi_definition:set_required_headers/2, [?KEY_CALL_ID
                                                            ,?KEY_CONTROL_QUEUE
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Custom-Channel-Vars">>
                                                            ,<<"Switch-Hostname">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{?KEY_CALL_ID, fun erlang:is_binary/1}
                ,{?KEY_CONTROL_QUEUE, fun erlang:is_binary/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Dialplan Route Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) -> kz_api:api_formatter_return().
req(Req) ->
    kapi_definition:build_message(Req, req_definition()).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Req) ->
    kapi_definition:validate(Req, req_definition()).

-spec req_event_type() -> {kz_term:ne_binary(), kz_term:ne_binary()}.
req_event_type() ->
    Definition = req_definition(),
    {kapi_definition:category(Definition), kapi_definition:name(Definition)}.

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, (kapi_definition:binding(Definition))(Req)).

%%------------------------------------------------------------------------------
%% @doc Dialplan Route Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
resp(Prop) ->
    Prop1 = case props:get_value(<<"Method">>, Prop) of
                <<"bridge">> ->
                    Routes = [begin
                                  {'ok', RouteProp} = resp_route(Route),
                                  kz_json:from_list(RouteProp)
                              end || Route <- props:get_value(<<"Routes">>, Prop)],
                    [{<<"Routes">>, Routes} | props:delete(<<"Routes">>, Prop)];
                _ ->
                    Prop
            end,
    kapi_definition:build_message(Prop1, resp_definition()).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Prop) ->
    Valid = kapi_definition:validate(Prop, resp_definition()),
    case props:get_value(<<"Method">>, Prop) of
        <<"bridge">> when Valid->
            lists:all(fun(Route) -> resp_route_v(Route) end
                     ,props:get_value(<<"Routes">>, Prop)
                     );
        _ ->
            Valid
    end.

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(RespQ, JObj) ->
    publish_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(RespQ, Resp, ContentType) ->
    Definition = resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec is_actionable_resp(kz_term:api_terms()) -> boolean().
is_actionable_resp(Prop) when is_list(Prop) ->
    case props:get_value(<<"Method">>, Prop) of
        <<"bridge">> -> 'true';
        <<"park">> -> 'true';
        <<"sms">> -> 'true';
        <<"plan">> -> 'true';
        <<"application">> -> 'true';
        <<"error">> -> 'true';
        _ -> 'false'
    end;
is_actionable_resp(JObj) ->
    is_actionable_resp(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Route within a Dialplan Route Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp_route(kz_term:api_terms()) -> kz_api:api_formatter_return().
resp_route(Prop) when is_list(Prop) ->
    case resp_route_v(Prop) of
        'true' -> kz_api:build_message_specific_headers(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?OPTIONAL_ROUTE_RESP_ROUTE_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_resp_route"}
    end;
resp_route(JObj) -> resp_route(kz_json:to_proplist(JObj)).

-spec resp_route_v(kz_term:api_terms()) -> boolean().
resp_route_v(Prop) when is_list(Prop) ->
    kz_api:validate_message(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?ROUTE_RESP_ROUTE_VALUES, ?ROUTE_RESP_ROUTE_TYPES);
resp_route_v(JObj) -> resp_route_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Winning Responder Message.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec win(kz_term:api_terms()) -> kz_api:api_formatter_return().
win(Req) ->
    kapi_definition:build_message(Req, win_definition()).

-spec win_v(kz_term:api_terms()) -> boolean().
win_v(Req) ->
    kapi_definition:validate(Req, win_definition()).

-spec publish_win(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_win(RespQ, JObj) ->
    publish_win(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_win(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_win(RespQ, Win, ContentType) ->
    Definition = win_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Win
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, props:get_value('restrict_to', Props), Props).

-spec bind_q(kz_term:ne_binary(), list() | 'undefined', kz_term:proplist()) -> 'ok'.
bind_q(Queue, 'undefined', Props) ->
    Keys = get_all_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:bind_q_to_callmgr(Queue, Key) end, Keys);
bind_q(Queue, ['no_account' | T], Props) ->
    Keys = get_realm_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:bind_q_to_callmgr(Queue, Key) end, Keys),
    bind_q(Queue, T, Props);
bind_q(Queue, ['account' | T], Props) ->
    Keys = get_account_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:bind_q_to_callmgr(Queue, Key) end, Keys),
    bind_q(Queue, T, Props);
bind_q(Queue, [_ | T], Props) ->
    bind_q(Queue, T, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, props:get_value('restrict_to', Props), Props).

-spec unbind_q(kz_term:ne_binary(), list() | 'undefined', kz_term:proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', Props) ->
    Keys = get_all_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:unbind_q_from_callmgr(Queue, Key) end, Keys);
unbind_q(Queue, ['no_account' | T], Props) ->
    Keys = get_realm_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:unbind_q_from_callmgr(Queue, Key) end, Keys),
    unbind_q(Queue, T, Props);
unbind_q(Queue, ['account' | T], Props) ->
    Keys = get_account_routing_keys(Props),
    lists:foreach(fun(Key) -> kz_amqp_util:unbind_q_from_callmgr(Queue, Key) end, Keys),
    unbind_q(Queue, T, Props);
unbind_q(Queue, [_ | T], Props) ->
    unbind_q(Queue, T, Props);
unbind_q(_, [], _) -> 'ok'.

get_all_routing_keys(Props) ->
    get_realm_routing_keys(Props) ++ get_account_routing_keys(Props).

get_realm_routing_keys(Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    User = props:get_value('user', Props, <<"*">>),
    Types = props:get_value('types', Props, [<<"*">>]),
    lists:foldl(fun(T, L) -> [get_route_req_realm_routing(T, Realm, User) | L] end, [], Types).

get_account_routing_keys(Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    Types = props:get_value('types', Props, [<<"*">>]),
    lists:foldl(fun(T, L) -> [get_route_req_account_routing(T, AccountId) | L] end, [], Types).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().

-spec get_route_req_account_routing(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_route_req_account_routing(Type, AccountId) ->
    list_to_binary([?KEY_ROUTE_REQ, ".", kz_amqp_util:encode(Type), ".", kz_amqp_util:encode(AccountId)]).

-spec get_route_req_realm_routing(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_route_req_realm_routing(Type, Realm, User) ->
    list_to_binary([?KEY_ROUTE_REQ, ".", kz_amqp_util:encode(Type), ".", kz_amqp_util:encode(Realm), ".", kz_amqp_util:encode(User)]).

-spec get_route_req_routing(kz_term:api_terms()) -> kz_term:ne_binary().
get_route_req_routing(API) ->
    {User, Realm} = get_auth_user_realm(API),
    Type = resource_type(API),
    case account_id(API) of
        'undefined' -> get_route_req_realm_routing(Type, Realm, User);
        AccountId -> get_route_req_account_routing(Type, AccountId)
    end.

%%------------------------------------------------------------------------------
%% @doc Extract the auth realm from the API request, using the requests to domain
%% when provided with an IP.
%% @end
%%------------------------------------------------------------------------------
-spec get_auth_realm(kz_term:api_terms()) -> kz_term:ne_binary().
get_auth_realm(ApiProp) when is_list(ApiProp) ->
    [_ReqUser, ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    ReqDomain;
get_auth_realm(ApiJObj) ->
    [_ReqUser, ReqDomain] = binary:split(kz_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqDomain.

-spec get_auth_user(kz_term:api_terms()) -> kz_term:ne_binary().
get_auth_user(ApiProp) when is_list(ApiProp) ->
    [ReqUser, _ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    ReqUser;
get_auth_user(ApiJObj) ->
    [ReqUser, _ReqDomain] = binary:split(kz_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqUser.

-spec get_auth_user_realm(kz_term:api_terms()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_auth_user_realm(ApiProp) when is_list(ApiProp) ->
    [ReqUser, ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    {ReqUser, ReqDomain};
get_auth_user_realm(ApiJObj) ->
    [ReqUser, ReqDomain] = binary:split(kz_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    {ReqUser, ReqDomain}.

-spec account_id(kz_term:api_terms()) -> kz_term:api_binary().
account_id(API) when is_list(API) ->
    account_id(kz_json:from_list(API));
account_id(API) ->
    kz_json:get_first_defined([<<"Account-ID">>
                              ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ]
                             ,API
                             ).

-spec resource_type(kz_term:api_terms()) -> kz_term:ne_binary().
resource_type(ApiProp) when is_list(ApiProp) ->
    props:get_value(<<"Resource-Type">>, ApiProp);
resource_type(ApiJObj) ->
    kz_json:get_value(<<"Resource-Type">>, ApiJObj).

-spec call_id(kz_json:object()) -> kz_term:api_binary().
call_id(JObj) ->
    kz_json:get_value(?KEY_CALL_ID, JObj).

-spec fetch_id(kz_json:object()) -> kz_term:api_binary().
fetch_id(JObj) ->
    kz_json:get_value(?KEY_FETCH_ID, JObj).

-spec control_queue(kz_json:object()) -> kz_term:api_binary().
control_queue(JObj) ->
    kz_json:get_value(?KEY_CONTROL_QUEUE, JObj).

-spec has_cost_parameters(kz_json:object()) -> boolean().
has_cost_parameters(JObj) ->
    kz_json:is_json_object(JObj)
        andalso kz_json:all(fun key_is_cost_param/1, JObj).

key_is_cost_param({K, _V}) ->
    lists:member(K, ?ROUTE_REQ_COST_PARAMS).
