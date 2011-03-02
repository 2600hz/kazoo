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

-define(APP_NAME, <<"cf_ring_group">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-define(DEFAULT_TIMEOUT, 30).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->    
    Endpoints = lists:foldr(fun(Endpoint, Acc) ->
                                    case get_endpoint(Endpoint) of
                                        {ok, E} -> [E|Acc];
                                        _ -> Acc
                                    end
                            end, [], whapps_json:get_value([<<"endpoints">>], Data, [])),
    bridge_endpoints(Endpoints, Data, Call),
    case wait_for_bridge() of
        {_, channel_hungup} ->
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the bridge call command
%% @end
%%--------------------------------------------------------------------
-spec(bridge_endpoints/3 :: (Endpoints :: list(json_object()), Data :: json_object(), Call :: #cf_call{}) -> ok).
bridge_endpoints(Endpoints, {struct, Props}, #cf_call{call_id=CallId} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"Timeout">>, get_value(<<"timeout">>, Props, ?DEFAULT_TIMEOUT)}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Dial-Endpoint-Method">>, get_value(<<"strategy">>, Props, <<"simultaneous">>)}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
            format_log(info, "SENT!~n~w~n", [Command]),
    {ok, Json} = whistle_api:bridge_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches a endpoint defintion from the database and returns the
%% whistle_api endpoint json
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint/1 :: (Data :: json_object()) -> tuple(ok, json_object()) | tuple(error, atom())).
get_endpoint({struct, Props}) ->
    Db = get_value(<<"database">>, Props),
    Id = get_value(<<"id">>, Props),
    case couch_mgr:open_doc(Db, Id) of
        {ok, Doc} ->
            Endpoint = [
                         {<<"Invite-Format">>, whapps_json:get_value(["sip", "invite-format"], Doc)}
                        ,{<<"To-User">>, whapps_json:get_value(["sip", "username"], Doc)}
                        ,{<<"To-Realm">>, whapps_json:get_value(["sip", "realm"], Doc)}
                        ,{<<"To-DID">>, whapps_json:get_value(["sip", "number"], Doc)}
                        ,{<<"Route">>, whapps_json:get_value(["sip", "url"], Doc)}
                        ,{<<"Ignore-Early-Media">>, whapps_json:get_value(["media", "ignore-early-media"], Doc)}
                        ,{<<"Bypass-Media">>, whapps_json:get_value(["media", "bypass-media"], Doc)}
                        ,{<<"Endpoint-Timeout">>, get_value(<<"timeout">>, Props, ?DEFAULT_TIMEOUT)}
                        ,{<<"Endpoint-Progress-Timeout">>, get_value(<<"progress-timeout">>, Props, <<"6">>)}
                        ,{<<"Endpoint-Delay">>, get_value(<<"delay">>, Props)}
                        ,{<<"Codecs">>, whapps_json:get_value(["media", "codecs"], Doc)}
                    ],
            {ok, {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Endpoint)}};
        {error, _}=E ->
            format_log(error, "CF_DEVICES(~w): Could not locate endpoint ~w in ~w (~w)~n", [self(), Id, Db, E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_bridge/0 :: () -> {ok|error, channel_hangup}|{error, bridge_failed|timeout}).    
wait_for_bridge() -> 
    receive
        {call_event, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_BRIDGE">>, _ } ->
                    wait_for_hangup();
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    {error, channel_hungup};
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">> } ->
                    {error, bridge_failed};
                _ ->
                    wait_for_bridge()
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When the bridge command is successfull this waits for the call to
%% hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_hangup/0 :: () -> {ok, channel_hangup}).    
wait_for_hangup() -> 
    receive
        {call_event, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    {ok, channel_hungup};
                _ ->
                    wait_for_hangup()
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_callctrl/2 :: (JSON :: json_object(), Call :: #cf_call{}) -> no_return()).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
