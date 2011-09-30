%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_conference).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(stop)).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, ctrl_q=CtrlQ, account_id=AccountId, amqp_q=AmqpQ}) ->
    put(callid, CallId),
    Command = [{<<"Account-ID">>, AccountId}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Control-Queue">>, CtrlQ}
               ,{<<"Conference-ID">>, wh_json:get_value(<<"id">>, Data)}
               ,{<<"Moderator">>, wh_json:get_binary_boolean(<<"moderator">>, Data)}
               | wh_api:default_headers(AmqpQ, <<"conference">>, <<"discovery">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_discovery_req(Command),
    amqp_util:conference_publish(Payload, discovery),
    {ok, _} = cf_call_command:wait_for_hangup(),
    CFPid ! {stop}.
