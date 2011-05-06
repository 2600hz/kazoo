%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_resources).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/4, wait_for_bridge/1, wait_for_unbridge/0]).

-define(VIEW_BY_RULES, <<"resources/listing_active_by_rules">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(_, #cf_call{cf_pid=CFPid}=Call) ->
    {ok, Gateways} = find_gateways(Call),
    bridge_to_gateways(Gateways, Call),
    CFPid ! {stop}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(bridge_to_gateways/2 :: (Resources :: proplist(), Call :: #cf_call{}) -> no_return()).
bridge_to_gateways([{DestNum, Gateways, CIDType}|T], Call) ->
    case b_bridge([create_endpoint(DestNum, Gtw) || Gtw <- Gateways], <<"60">>, CIDType, Call) of
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
-spec(create_endpoint/2 :: (DestNum :: binary(), Gateway :: json_object()) -> json_object()).
create_endpoint(DestNum, JObj) ->
    Rule = <<"sip:"
              ,(wh_json:get_value(<<"prefix">>, JObj, <<>>))/binary
              ,DestNum/binary
              ,(wh_json:get_value(<<"suffix">>, JObj, <<>>))/binary
              ,$@ ,(wh_json:get_value(<<"server">>, JObj))/binary>>,
    Endpoint = [
                 {<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Rule}
                ,{<<"Auth-User">>, wh_json:get_value(<<"username">>, JObj)}
                ,{<<"Auth-Password">>, wh_json:get_value(<<"password">>, JObj)}
                ,{<<"Bypass-Media">>, wh_json:get_value(<<"bypass_media">>, JObj)}
                ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, JObj, <<"6">>)}
                ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, JObj)}
               ],
    {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Endpoint)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(find_gateways/1 :: (Call :: #cf_call{}) -> tuple(ok, proplist()) | tuple(error, atom())).
find_gateways(#cf_call{account_db=Db, dest_number=DestNum}) ->
    case couch_mgr:get_results(Db, ?VIEW_BY_RULES, []) of
        {ok, Resources} ->
            {ok, [ {Number
                    ,wh_json:get_value([<<"value">>, <<"gateways">>], Resource, [])
                    ,wh_json:get_value([<<"value">>, <<"caller_id_options">>, <<"type">>], Resource, <<"external">>)}
                   || Resource <- Resources
			 , Number <- evaluate_rules(wh_json:get_value(<<"key">>, Resource), DestNum)
			 , Number =/= []
                 ]};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_rules/2 :: (Key :: list(), DestNum:: binary()) -> list()).
evaluate_rules([_, Regex], DestNum) ->
    case re:run(DestNum, Regex) of
        {match, [_, {Start,End}|_]} ->
            [binary:part(DestNum, Start, End)];
        {match, _} ->
            [DestNum];
        _ -> []
    end.
