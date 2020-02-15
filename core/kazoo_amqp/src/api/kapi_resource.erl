%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_resource).

-export([api_definitions/0, api_definition/1]).

-export([originate_req/1
        ,originate_req_v/1
        ,publish_originate_req/1
        ,publish_originate_req/2
        ]).
-export([originate_resp/1
        ,originate_resp_v/1
        ,publish_originate_resp/2
        ,publish_originate_resp/3
        ]).
-export([originate_started/1
        ,originate_started_v/1
        ,publish_originate_started/2
        ,publish_originate_started/3
        ]).
-export([originate_uuid/1
        ,originate_uuid_v/1
        ,publish_originate_uuid/2
        ,publish_originate_uuid/3
        ]).
-export([eavesdrop_req/1
        ,eavesdrop_req_v/1
        ,publish_eavesdrop_req/1
        ,publish_eavesdrop_req/2
        ]).
-export([eavesdrop_resp/1
        ,eavesdrop_resp_v/1
        ,publish_eavesdrop_resp/2
        ,publish_eavesdrop_resp/3
        ]).

-export([is_valid_mode/1
        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ]).

-export([originate_ready/1, originate_ready_v/1
        ,originate_execute/1, originate_execute_v/1
        ]).

-include("kz_amqp_util.hrl").

%% Eavesdrop: If you set a Group ID, the Call-ID is ignored and "all" is used instead
-define(EAVESDROP_VALID_MODES, [<<"listen">>   % hear both sides - default
                               ,<<"whisper">> % talk to one side
                               ,<<"full">>    % talk to both sides
                               ]).
-define(EAVESDROP_MODE, {<<"Eavesdrop-Mode">>, ?EAVESDROP_VALID_MODES}).

%% Originate Endpoints
-define(ORIGINATE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_ORIGINATE_REQ_ENDPOINT_HEADERS, kapi_dialplan:optional_bridge_req_endpoint_headers()).
-define(ORIGINATE_REQ_ENDPOINT_VALUES, [{<<"Endpoint-Type">>, [<<"sip">>, <<"freetdm">>]}]).
-define(ORIGINATE_REQ_ENDPOINT_TYPES, [{<<"Bypass-Media">>, fun kz_term:is_boolean/1}
                                      ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                                      ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                      ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                                      ,{<<"Endpoint-Options">>, fun kz_json:is_json_object/1}
                                      ,{<<"Ignore-Early-Media">>, fun kz_term:is_boolean/1}
                                      ]).

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [originate_req_definition()
    ,originate_resp_definition()
    ,originate_started_definition()
    ,originate_uuid_definition()
    ,eavesdrop_req_definition()
    ,eavesdrop_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"originate_req">>) ->
    originate_req_definition();
api_definition(<<"originate_resp">>) ->
    originate_resp_definition();
api_definition(<<"originate_started">>) ->
    originate_started_definition();
api_definition(<<"originate_uuid">>) ->
    originate_uuid_definition();
api_definition(<<"eavesdrop_req">>) ->
    eavesdrop_req_definition();
api_definition(<<"eavesdrop_resp">>) ->
    eavesdrop_resp_definition().

-spec originate_req_definition() -> kapi_definition:api().
originate_req_definition() ->
    EventName = <<"originate_req">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resource Request">>}
              ,{fun kapi_definition:set_description/2, <<"Resource Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_req/1}
              ,{fun kapi_definition:set_binding/2, <<"originate.resource.req">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Application-Name">>
                                                            ,<<"Endpoints">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Application-Data">>
                                                            ,<<"Custom-Application-Vars">>
                                                            ,<<"Custom-Channel-Vars">>
                                                                 %% Eavesdrop
                                                            ,<<"Eavesdrop-Call-ID">>
                                                            ,<<"Eavesdrop-Group-ID">>
                                                            ,<<"Eavesdrop-Mode">>

                                                            ,<<"Existing-Call-ID">> % If set, use this node, otherwise ignore
                                                            ,<<"Export-Custom-Channel-Vars">>
                                                            ,<<"Fax-Identity-Name">>
                                                            ,<<"Fax-Identity-Number">>
                                                            ,<<"Fax-Timezone">>
                                                            ,<<"Intercept-Unbridged-Only">>
                                                            ,<<"Loopback-Bowout">>
                                                            ,<<"Origination-Call-ID">>
                                                            ,<<"Originate-Immediate">>
                                                            ,<<"Outbound-Call-ID">>
                                                            ,<<"Simplify-Loopback">> %% loopback_bowout flag
                                                            ,<<"Start-Control-Process">>
                                                                 | kapi_dialplan:optional_bridge_req_headers()
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Application-Name">>, [<<"bridge">>
                                          ,<<"eavesdrop">>
                                          ,<<"fax">>
                                          ,<<"park">>
                                          ,<<"transfer">>
                                          ]}
                ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                ,?EAVESDROP_MODE
                ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Continue-On-Fail">>, fun kz_term:is_boolean/1}
                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                ,{<<"Endpoints">>, fun is_list/1}
                ,{<<"Simplify-Bowout">>, fun kz_term:is_boolean/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec originate_resp_definition() -> kapi_definition:api().
originate_resp_definition() ->
    EventName = <<"originate_resp">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resource Response">>}
              ,{fun kapi_definition:set_description/2, <<"Resource Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Channel-Call-State">>
                     | kapi_call:optional_call_event_headers()
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec originate_started_definition() -> kapi_definition:api().
originate_started_definition() ->
    EventName = <<"originate_started">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resource Request started">>}
              ,{fun kapi_definition:set_description/2, <<"Resource Request started">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_started/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_started_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_started/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Channel-Call-State">>
                     | kapi_call:optional_call_event_headers()
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec originate_uuid_definition() -> kapi_definition:api().
originate_uuid_definition() ->
    EventName = <<"originate_uuid">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resource Request UUID">>}
              ,{fun kapi_definition:set_description/2, <<"Resource Request UUID">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun originate_uuid/1}
              ,{fun kapi_definition:set_validate_fun/2, fun originate_uuid_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_originate_uuid/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Outbound-Call-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Outbound-Call-Control-Queue">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec eavesdrop_req_definition() -> kapi_definition:api().
eavesdrop_req_definition() ->
    EventName = <<"eavesdrop_req">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Eavesdrop Request">>}
              ,{fun kapi_definition:set_description/2, <<"Eavesdrop Request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun eavesdrop_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun eavesdrop_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_eavesdrop_req/1}
              ,{fun kapi_definition:set_binding/2, <<"eavesdrop.resource.req">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Endpoint-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Eavesdrop-Call-ID">>
                ,<<"Eavesdrop-Group-ID">>
                ,<<"Eavesdrop-Mode">>
                     | kapi_definition:required_headers(originate_req_definition())
                ]
               }
              ,{fun kapi_definition:set_values/2
               ,[?EAVESDROP_MODE
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec eavesdrop_resp_definition() -> kapi_definition:api().
eavesdrop_resp_definition() ->
    EventName = <<"eavesdrop_resp">>,
    Category = <<"resource">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Eavesdrop Response">>}
              ,{fun kapi_definition:set_description/2, <<"Eavesdrop Response">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun eavesdrop_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun eavesdrop_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_eavesdrop_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Eavesdropper-Call-ID">>
                                                            ,<<"Error-Msg">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Status">>, [<<"started">>, <<"error">>]}
                 | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec originate_ready(kz_term:api_terms()) -> api_formatter_return().
originate_ready(API) ->
    kapi_dialplan:originate_ready(API).

-spec originate_ready_v(kz_term:api_terms()) -> boolean().
originate_ready_v(API) ->
    kapi_dialplan:originate_ready_v(API).

-spec originate_execute(kz_term:api_terms()) -> api_formatter_return().
originate_execute(API) ->
    kapi_dialplan:originate_execute(API).

-spec originate_execute_v(kz_term:api_terms()) -> boolean().
originate_execute_v(API) ->
    kapi_dialplan:originate_execute_v(API).

%%------------------------------------------------------------------------------
%% @doc Resource Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_req(Prop) ->
    EPs = [update_originate_endpoint(EP)
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              originate_req_endpoint_v(EP)
          ],
    Prop1 = [{<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    kapi_definition:build_message(Prop1, originate_req_definition()).

-spec update_originate_endpoint(kz_json:object()) -> kz_json:object().
update_originate_endpoint(EP) ->
    {'ok', EPProps} = originate_req_endpoint_headers(EP),
    kz_json:from_list(EPProps).

-spec originate_req_v(kz_term:api_terms()) -> boolean().
originate_req_v(Req) ->
    kapi_definition:validate(Req, originate_req_definition()).

-spec originate_req_endpoint_headers(kz_term:api_terms()) -> api_formatter_return().
originate_req_endpoint_headers(Prop) when is_list(Prop) ->
    kz_api:build_message_specific_headers(Prop, ?ORIGINATE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_ORIGINATE_REQ_ENDPOINT_HEADERS);
originate_req_endpoint_headers(JObj) ->
    originate_req_endpoint_headers(kz_json:to_proplist(JObj)).

-spec originate_req_endpoint_v(kz_term:api_terms()) -> boolean().
originate_req_endpoint_v(Prop) when is_list(Prop) ->
    kz_api:validate_message(Prop, ?ORIGINATE_REQ_ENDPOINT_HEADERS, ?ORIGINATE_REQ_ENDPOINT_VALUES, ?ORIGINATE_REQ_ENDPOINT_TYPES);
originate_req_endpoint_v(JObj) ->
    originate_req_endpoint_v(kz_json:to_proplist(JObj)).

-spec publish_originate_req(kz_term:api_terms()) -> 'ok'.
publish_originate_req(JObj) ->
    publish_originate_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_req(Req, ContentType) ->
    Definition = originate_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload
                                ,ContentType
                                ,kapi_definition:binding(Definition)
                                ,[{'mandatory', 'true'}]
                                ).

%%------------------------------------------------------------------------------
%% @doc Resource Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_resp(Req) ->
    kapi_definition:build_message(Req, originate_resp_definition()).

-spec originate_resp_v(kz_term:api_terms()) -> boolean().
originate_resp_v(Req) ->
    kapi_definition:validate(Req, originate_resp_definition()).

-spec publish_originate_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_resp(TargetQ, JObj) ->
    publish_originate_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_resp(TargetQ, Resp, ContentType) ->
    Definition = originate_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Resource Request started.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_started(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_started(Req) ->
    kapi_definition:build_message(Req, originate_started_definition()).

-spec originate_started_v(kz_term:api_terms()) -> boolean().
originate_started_v(Req) ->
    kapi_definition:validate(Req, originate_started_definition()).

-spec publish_originate_started(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_started(TargetQ, JObj) ->
    publish_originate_started(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_started(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_started(TargetQ, Resp, ContentType) ->
    Definition = originate_started_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Resource Request UUID.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec originate_uuid(kz_term:api_terms()) -> kz_api:api_formatter_return().
originate_uuid(Req) ->
    kapi_definition:build_message(Req, originate_uuid_definition()).

-spec originate_uuid_v(kz_term:api_terms()) -> boolean().
originate_uuid_v(Req) ->
    kapi_definition:validate(Req, originate_uuid_definition()).

-spec publish_originate_uuid(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_originate_uuid(TargetQ, JObj) ->
    publish_originate_uuid(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_originate_uuid(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_originate_uuid(TargetQ, Resp, ContentType) ->
    Definition = originate_uuid_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Eavesdrop Request.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec eavesdrop_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
eavesdrop_req(Req) ->
    kapi_definition:build_message(Req, eavesdrop_req_definition()).

-spec eavesdrop_req_v(kz_term:api_terms()) -> boolean().
eavesdrop_req_v(Req) ->
    kapi_definition:validate(Req, eavesdrop_req_definition()).

-spec publish_eavesdrop_req(kz_term:api_terms()) -> 'ok'.
publish_eavesdrop_req(JObj) ->
    publish_eavesdrop_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_eavesdrop_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_eavesdrop_req(Req, ContentType) ->
    Definition = eavesdrop_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:callmgr_publish(Payload, ContentType, kapi_definition:binding(Definition)).

%%------------------------------------------------------------------------------
%% @doc Eavesdrop Response.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec eavesdrop_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
eavesdrop_resp(Req) ->
    kapi_definition:build_message(Req, eavesdrop_resp_definition()).

-spec eavesdrop_resp_v(kz_term:api_terms()) -> boolean().
eavesdrop_resp_v(Req) ->
    kapi_definition:validate(Req, eavesdrop_resp_definition()).

-spec publish_eavesdrop_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_eavesdrop_resp(TargetQ, JObj) ->
    publish_eavesdrop_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_eavesdrop_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_eavesdrop_resp(TargetQ, Resp, ContentType) ->
    Definition = eavesdrop_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

-spec is_valid_mode(kz_term:ne_binary()) -> boolean().
is_valid_mode(M) ->
    lists:member(M, ?EAVESDROP_VALID_MODES).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Prop) ->
    bind_q(Queue, Prop, props:get_value('restrict_to', Prop)).

bind_q(Queue, _Prop, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(originate_req_definition())),
    kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(eavesdrop_req_definition()));
bind_q(Queue, Prop, ['originate'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(originate_req_definition())),
    bind_q(Queue, Prop, T);
bind_q(Queue, Prop, ['eavesdrop'|T]) ->
    'ok' = kz_amqp_util:bind_q_to_callmgr(Queue, kapi_definition:binding(eavesdrop_req_definition())),
    bind_q(Queue, Prop, T);
bind_q(Queue, Prop, [_|T]) ->
    bind_q(Queue, Prop, T);
bind_q(_, _, []) ->
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Prop) ->
    unbind_q(Queue, Prop, props:get_value('restrict_to', Prop)).

unbind_q(Queue, _Prop, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(originate_req_definition())),
    kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(eavesdrop_req_definition()));
unbind_q(Queue, Prop, ['originate'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(originate_req_definition())),
    unbind_q(Queue, Prop, T);
unbind_q(Queue, Prop, ['eavesdrop'|T]) ->
    'ok' = kz_amqp_util:unbind_q_from_callmgr(Queue, kapi_definition:binding(eavesdrop_req_definition())),
    unbind_q(Queue, Prop, T);
unbind_q(Queue, Prop, [_|T]) ->
    unbind_q(Queue, Prop, T);
unbind_q(_, _, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:callmgr_exchange().
