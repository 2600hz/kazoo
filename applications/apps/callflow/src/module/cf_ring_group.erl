%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_ring_group).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/6, wait_for_bridge/1, wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(stop | continue)).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    Endpoints = lists:foldr(fun(Member, Acc) ->
                                    case cf_endpoint:build(wh_json:get_value(<<"id">>, Member), Call) of
                                        {ok, {struct, Props}=E} -> 
                                            case wh_json:get_value(<<"delay">>, Member) of
                                                undefined -> [E|Acc];
                                                Delay -> [{struct, [{<<"Endpoint-Delay">>, Delay}|Props]}|Acc]
                                            end;
                                        _ -> Acc
                                    end
                            end, [], wh_json:get_value([<<"endpoints">>], Data, [])),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_value(<<"strategy">>, Data, <<"simultaneous">>),
    case b_bridge(Endpoints, Timeout, {undefined, undefined}, Strategy, <<"true">>, Call) of
        {ok, _} ->
            get_channel_status(Call),
            _ = wait_for_unbridge(),
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.

-spec(get_channel_status/1 :: (Call :: #cf_call{}) -> no_return()).
get_channel_status(#cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call_event">>, <<"status_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:call_status_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    cf_call_common:send_callctrl(Payload, Call),
    wait_for_channel_status().

wait_for_channel_status() ->
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case wh_json:get_value(<<"Event-Name">>, JObj) of
                <<"status_resp">> ->
                    logger:format_log(info, "CHANNEL_STATUS: ~p", [JObj]);
                _ ->
                    wait_for_channel_status()
            end
    after 5000 ->
            ok
    end.
