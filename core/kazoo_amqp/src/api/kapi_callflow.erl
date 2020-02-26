%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_callflow).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include("kz_api_literals.hrl").

-export([api_definitions/0, api_definition/1]).

-export([resume/1
        ,resume_v/1
        ,publish_resume/1
        ]).
-export([action_execute/1
        ,action_execute_v/1
        ,publish_action_execute/2
        ]).
-export([action_accepted/1
        ,action_accepted_v/1
        ,publish_action_accepted/2
        ]).
-export([action_result/1
        ,action_result_v/1
        ,publish_action_result/2
        ]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).

-type resume() :: kz_json:object().

-export_type([resume/0]).

-ifdef(TEST).
-export([action_routing_key/1
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [resume_definition()
    ,action_execute_definition()
    ,action_accepted_definition()
    ,action_result_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"resume">>) ->
    resume_definition();
api_definition(<<"action_execute">>) ->
    action_execute_definition();
api_definition(<<"action_accepted">>) ->
    action_accepted_definition();
api_definition(<<"action_result">>) ->
    action_result_definition().

-spec resume_definition() -> kapi_definition:api().
resume_definition() ->
    EventName = <<"resume">>,
    Category = <<"callflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Resume Callflow">>}
              ,{fun kapi_definition:set_description/2, <<"Resume a Callflow's flow">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun resume/1}
              ,{fun kapi_definition:set_validate_fun/2, fun resume_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_resume/1}
              ,{fun kapi_definition:set_binding/2, <<"callflow.resume">>}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call">>
                                                            ,<<"Flow">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec action_execute_definition() -> kapi_definition:api().
action_execute_definition() ->
    EventName = <<"action.execute">>,
    Category = <<"callflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_description/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun action_execute/1}
              ,{fun kapi_definition:set_validate_fun/2, fun action_execute_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_action_execute/2}
              ,{fun kapi_definition:set_binding/2, fun action_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Call-ID">>
                                                            ,<<"Call">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Data">>]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec action_accepted_definition() -> kapi_definition:api().
action_accepted_definition() ->
    EventName = <<"action.accepted">>,
    Category = <<"callflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_description/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun action_accepted/1}
              ,{fun kapi_definition:set_validate_fun/2, fun action_accepted_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_action_accepted/2}
              ,{fun kapi_definition:set_required_headers/2, [?KEY_MSG_ID
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec action_result_definition() -> kapi_definition:api().
action_result_definition() ->
    EventName = <<"action.result">>,
    Category = <<"callflow">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_description/2, <<"Remote execute a Callflow action">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun action_result/1}
              ,{fun kapi_definition:set_validate_fun/2, fun action_result_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_action_result/2}
              ,{fun kapi_definition:set_required_headers/2, [?KEY_MSG_ID
                                                            ,<<"Call-ID">>
                                                            ,<<"Result">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Resume a Callflow's flow.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resume(kz_term:api_terms()) -> kz_api:api_formatter_return().
resume(Req) ->
    kapi_definition:build_message(Req, resume_definition()).

-spec resume_v(kz_term:api_terms()) -> boolean().
resume_v(Req) ->
    kapi_definition:validate(Req, resume_definition()).

-spec publish_resume(kz_term:api_terms()) -> 'ok'.
publish_resume(JObj) ->
    Definition = resume_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:kapps_publish(kapi_definition:binding(Definition), Payload).

%%------------------------------------------------------------------------------
%% @doc Remote execution of a Callflow's action.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec action_execute(kz_term:api_terms()) -> kz_api:api_formatter_return().
action_execute(Req) ->
    kapi_definition:build_message(Req, action_execute_definition()).

-spec action_execute_v(kz_term:api_terms()) -> boolean().
action_execute_v(Req) ->
    kapi_definition:validate(Req, action_execute_definition()).

-spec publish_action_execute(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_action_execute(Action, API) ->
    Definition = action_execute_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    Options = [{mandatory, true}],
    kz_amqp_util:kapps_publish((kapi_definition:binding(action_execute_definition()))(Action)
                              ,Payload
                              ,?DEFAULT_CONTENT_TYPE
                              ,Options
                              ).

-spec action_accepted(kz_term:api_terms()) -> kz_api:api_formatter_return().
action_accepted(Req) ->
    kapi_definition:build_message(Req, action_accepted_definition()).

-spec action_accepted_v(kz_term:api_terms()) -> boolean().
action_accepted_v(Req) ->
    kapi_definition:validate(Req, action_accepted_definition()).

-spec publish_action_accepted(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_action_accepted(ServerId, JObj) ->
    Definition = action_accepted_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload).

-spec action_result(kz_term:api_terms()) -> kz_api:api_formatter_return().
action_result(Req) ->
    kapi_definition:build_message(Req, action_result_definition()).

-spec action_result_v(kz_term:api_terms()) -> boolean().
action_result_v(Req) ->
    kapi_definition:validate(Req, action_result_definition()).

-spec publish_action_result(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_action_result(ServerId, JObj) ->
    Definition = action_result_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,kapi_definition:values(Definition)
                                                ,[{'formatter', kapi_definition:build_fun(Definition)}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:targeted_publish(ServerId, Payload).

%%------------------------------------------------------------------------------
%% @doc Binds used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    bind_q(Q, RestrictTo, Props).

-spec bind_q(kz_term:ne_binary(), kz_term:api_atoms(), kz_term:proplist()) -> 'ok'.
bind_q(Q, 'undefined', _) ->
    kz_amqp_util:bind_q_to_kapps(Q, kapi_definition:binding(resume_definition()));
bind_q(Q, ['resume'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_kapps(Q, kapi_definition:binding(resume_definition())),
    bind_q(Q, Restrict, Props);
bind_q(Q, ['actions'|Restrict], Props) ->
    Actions = props:get_value('actions', Props, []),
    _ = [kz_amqp_util:bind_q_to_kapps(Q
                                     ,(kapi_definition:binding(action_execute_definition()))(kz_term:to_binary(Action))
                                     )
         || Action <- Actions
        ],
    bind_q(Q, Restrict, Props);
bind_q(Q, [_|Restrict], Props) ->
    bind_q(Q, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    unbind_q(Q, RestrictTo, Props).

-spec unbind_q(kz_term:ne_binary(), kz_term:api_atoms(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, 'undefined', _) ->
    kz_amqp_util:unbind_q_from_kapps(Q, kapi_definition:binding(resume_definition()));
unbind_q(Q, ['resume'|Restrict], Props) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, kapi_definition:binding(resume_definition())),
    unbind_q(Q, Restrict, Props);
unbind_q(Q, ['actions'|Restrict], Props) ->
    Actions = props:get_value('actions', Props, []),
    _ = [kz_amqp_util:unbind_q_from_kapps(Q
                                         ,(kapi_definition:binding(action_execute_definition()))(kz_term:to_binary(Action))
                                         )
         || Action <- Actions
        ],
    unbind_q(Q, Restrict, Props);
unbind_q(Q, [_|Restrict], Props) ->
    unbind_q(Q, Restrict, Props);
unbind_q(_, [], _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange(),
    kz_amqp_util:targeted_exchange().

-spec action_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
action_routing_key(Action) ->
    <<"callflow.action.", Action/binary>>.
