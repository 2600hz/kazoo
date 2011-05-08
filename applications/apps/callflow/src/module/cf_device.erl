%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_device).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/6, wait_for_bridge/1, wait_for_unbridge/0, set/3]).

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
    {ok, Endpoint} = cf_endpoint:build(wh_json:get_value(<<"id">>, Data), Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),  
    IgnoreEarlyMedia = wh_json:get_value(<<"Ignore-Early-Media">>, Endpoint),
    case b_bridge([Endpoint], Timeout, {undefined, undefined}, <<"single">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            update_call_realm(wh_json:get_value([<<"Custom-Call-Vars">>, <<"Realm">>], Endpoint), Call),
            _ = wait_for_unbridge(),
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When the bridge is successfull this is used to set the realm of 
%% the endpoint on the a-leg.  This is necessary, for example, in 
%% blind transfers to external numbers where the a-leg is seen
%% as the 'orginator'. 
%% @end
%%--------------------------------------------------------------------
-spec(update_call_realm/2 :: (Realm :: binary() | undefined, Call :: #cf_call{}) -> no_return()).
update_call_realm(undefined, _) ->
    ok;
update_call_realm(Realm, Call) ->
    set(undefined, {struct, [{<<"Realm">>, Realm}]}, Call).
