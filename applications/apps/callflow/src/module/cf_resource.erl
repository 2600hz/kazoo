%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_resource).

-include("../callflow.hrl").

-export([handle/2]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-import(cf_call_command, [b_bridge/3, wait_for_bridge/1, wait_for_unbridge/0]).

-define(VIEW_BY_ROUTE, <<"resources/listing_active_by_route">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    {ok, Gateways} = find_gateways(whapps_json:get_value(<<"database">>, Data), Call),
    bridge_to_gateways(Gateways, Call),
    CFPid ! {stop}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(bridge_to_gateways/2 :: (Resources :: proplist(), Call :: #cf_call{}) -> ok).                                    
bridge_to_gateways([{To, Gateways}|T], Call) ->
    case b_bridge([create_endpoint(To, Gtw) || Gtw <- Gateways], <<"120">>, Call) of
        {ok, _} ->
            wait_for_unbridge();
        {error, _} ->
            bridge_to_gateways(T, Call)
    end.       

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(create_endpoint/2 :: (To :: binary(), Gateway :: json_object()) -> json_object()).                                 
create_endpoint(To, JObj) ->  
    Route = <<"sip:"
              ,(whapps_json:get_value(<<"prefix">>, JObj, <<>>))/binary 
              ,To/binary
              ,(whapps_json:get_value(<<"suffix">>, JObj, <<>>))/binary 
              ,$@ ,(whapps_json:get_value(<<"server">>, JObj))/binary>>,
    Endpoint = [
                 {<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Route}
                ,{<<"Auth-User">>, whapps_json:get_value(<<"username">>, JObj)}
                ,{<<"Auth-Password">>, whapps_json:get_value(<<"password">>, JObj)}
                ,{<<"Bypass-Media">>, whapps_json:get_value(<<"bypass-media">>, JObj)}
                ,{<<"Endpoint-Progress-Timeout">>, get_value(<<"progress-timeout">>, JObj, <<"6">>)}
                ,{<<"Codecs">>, whapps_json:get_value(<<"codecs">>, JObj)}
               ],
    {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Endpoint)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(find_gateways/2 :: (Db :: binary(), Call :: #cf_call{}) -> tuple(ok, json_objects()) | tuple(error, atom())).                               
find_gateways(Db, #cf_call{to_number=To}) ->
    case couch_mgr:get_results(Db, ?VIEW_BY_ROUTE, []) of
        {ok, Resources} ->
            {ok, [ {Number, whapps_json:get_value([<<"value">>, <<"gateways">>], Resource, [])} ||
                      Resource <- Resources
                     ,Number <- evaluate_route(whapps_json:get_value(<<"key">>, Resource), To)
                     ,Number =/= []
                 ]}; 
        {error, _}=E ->
            E                   
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_route/2 :: (Key :: list(), To :: binary()) -> list()).                             
evaluate_route([_, Regex], To) ->
    try
        {match, Number} = re:run(To, Regex, [{capture, [1], binary}]),
        case Number of [<<>>] -> [To]; Else -> Else end
    catch
        _:_ -> []
    end.
