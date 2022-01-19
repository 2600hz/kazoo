%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_callflow).
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([resume/1, resume_v/1]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).
-export([publish_resume/1]).

-define(RESUME_ROUTING_KEY, <<"callflow.resume">>).

-define(RESUME_HEADERS, [<<"Call">>, <<"Flow">>]).
-define(OPTIONAL_RESUME_HEADERS, []).
-define(RESUME_VALUES, [{<<"Event-Category">>, <<"callflow">>}
                       ,{<<"Event-Name">>, <<"resume">>}
                       ]).
-define(RESUME_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Resume a Callflow's flow.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resume(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
resume(Prop) when is_list(Prop) ->
    case resume_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESUME_HEADERS, ?OPTIONAL_RESUME_HEADERS);
        'false' -> {'error', "Proplist failed validation for authn_error"}
    end;
resume(JObj) -> resume(kz_json:to_proplist(JObj)).

-spec resume_v(kz_term:api_terms()) -> boolean().
resume_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESUME_HEADERS, ?RESUME_VALUES, ?RESUME_TYPES);
resume_v(JObj) -> resume_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    kz_amqp_util:bind_q_to_kapps(Q, ?RESUME_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    kz_amqp_util:unbind_q_from_kapps(Q, ?RESUME_ROUTING_KEY).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish the JSON string to the proper Exchange.
%% @end
%%------------------------------------------------------------------------------
-spec publish_resume(kz_term:api_terms()) -> 'ok'.
publish_resume(JObj) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj
                                                ,?RESUME_VALUES
                                                ,[{'formatter', fun resume/1}
                                                 ,{'remove_recursive', 'false'}
                                                 ]
                                                ),
    kz_amqp_util:kapps_publish(?RESUME_ROUTING_KEY, Payload).
