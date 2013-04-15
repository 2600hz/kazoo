%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_callflow).

-export([resume/1, resume_v/1]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([publish_resume/1, publish_resume/2]).

-include("callflow.hrl").

-define(RESUME_ROUTING_KEY, <<"callflow.resume">>).

-define(RESUME_HEADERS, [<<"Call">>, <<"Flow">>]).
-define(OPTIONAL_RESUME_HEADERS, []).
-define(RESUME_VALUES, [{<<"Event-Category">>, <<"callflow">>}
                        ,{<<"Event-Name">>, <<"resume">>}
                       ]).
-define(RESUME_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Resume a callflow's flow
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec resume(api_terms()) -> {'ok', iolist()} | {'error', string()}.
resume(Prop) when is_list(Prop) ->
    case resume_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RESUME_HEADERS, ?OPTIONAL_RESUME_HEADERS);
        'false' -> {'error', "Proplist failed validation for authn_error"}
    end;
resume(JObj) -> resume(wh_json:to_proplist(JObj)).

-spec resume_v(api_terms()) -> boolean().
resume_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESUME_HEADERS, ?RESUME_VALUES, ?RESUME_TYPES);
resume_v(JObj) -> resume_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:whapps_exchange(),
    amqp_util:bind_q_to_whapps(Q, ?RESUME_ROUTING_KEY).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_whapps(Q, ?RESUME_ROUTING_KEY).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_resume(api_terms()) -> 'ok'.
-spec publish_resume(api_terms(), binary()) -> 'ok'.
publish_resume(JObj) ->
    publish_resume(JObj, ?DEFAULT_CONTENT_TYPE).
publish_resume(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?RESUME_VALUES, fun ?MODULE:resume/1),
    amqp_util:whapps_publish(Payload, ContentType, ?RESUME_ROUTING_KEY).
