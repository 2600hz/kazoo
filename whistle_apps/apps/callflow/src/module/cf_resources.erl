%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
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
handle(_, #cf_call{call_id=CallId}=Call) ->
    put(callid, CallId),
    {ok, Gateways} = find_gateways(Call),
    bridge_to_gateways(Gateways, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the gateways in a resource attempting each till a bridge
%% is successfull (not necessarly the call but the bridge).
%%
%% When this function gets to the end of the resource list this function
%% will not match, causing the process to crash and the callflow to
%% advanced, because its cool like that
%% @end
%%--------------------------------------------------------------------
-spec(bridge_to_gateways/2 :: (Resources :: proplist(), Call :: #cf_call{}) -> no_return()).
bridge_to_gateways([{DestNum, Gateways, CIDType}|T], #cf_call{cf_pid=CFPid}=Call) ->
    case b_bridge([create_endpoint(DestNum, Gtw) || Gtw <- Gateways], <<"60">>, CIDType, Call) of
        {ok, _} ->
            ?LOG("resource acquired"),
            wait_for_unbridge(),
            ?LOG("resource released"),
            CFPid ! {stop};
        {error, {bridge_failed, R}} ->
            ?LOG("resource failed ~s", [R]),
            bridge_to_gateways(T, Call);
        {error, R} ->
            ?LOG("resource failed ~w", [R]),
            bridge_to_gateways(T, Call)
    end;
bridge_to_gateways([], #cf_call{cf_pid=CFPid}) ->
    ?LOG("resources exhausted without success"),
    CFPid ! {continue}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This will build endpoint from the giving resource/rule combination
%% for use with the whistle bridge API.
%% @end
%%--------------------------------------------------------------------
-spec(create_endpoint/2 :: (DestNum :: binary(), Gateway :: json_object()) -> json_object()).
create_endpoint(DestNum, JObj) ->
    Rule = <<"sip:"
              ,(wh_json:get_value(<<"prefix">>, JObj, <<>>))/binary
              ,DestNum/binary
              ,(wh_json:get_value(<<"suffix">>, JObj, <<>>))/binary
              ,$@ ,(wh_json:get_value(<<"server">>, JObj))/binary>>,
    ?LOG("attempting resource ~s", [Rule]),
    Endpoint = [
                 {<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Rule}
                ,{<<"Auth-User">>, wh_json:get_value(<<"username">>, JObj)}
                ,{<<"Auth-Password">>, wh_json:get_value(<<"password">>, JObj)}
                ,{<<"Bypass-Media">>, wh_json:get_value(<<"bypass_media">>, JObj)}
                ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, JObj, <<"6">>)}
                ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, JObj)}
               ],
    {struct, [ KV || {_, V}=KV <- Endpoint, V =/= undefined ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve a complete list of resources in this database that are
%% enabled.  Remove any where the rules do not apply, and the destination
%% number as formated by that rule (ie: capture group or full number).
%% @end
%%--------------------------------------------------------------------
-spec(find_gateways/1 :: (Call :: #cf_call{}) -> tuple(ok, proplist()) | tuple(error, atom())).
find_gateways(#cf_call{account_db=Db, request_user=ReqNum}=Call) ->
    ?LOG("searching for resources"),
    case couch_mgr:get_results(Db, ?VIEW_BY_RULES, []) of
        {ok, Resources} ->
            ?LOG("found resources, filtering by rules"),
            {ok, [ {Number
                    ,wh_json:get_value([<<"value">>, <<"gateways">>], Resource, [])
                    ,get_caller_id_type(Resource, Call)}
                   || Resource <- Resources
			 , Number <- evaluate_rules(wh_json:get_value(<<"key">>, Resource), ReqNum)
			 , Number =/= []
                 ]};
        {error, R}=E ->
            ?LOG("search failed ~w", [R]),
            E
    end.

-spec(get_caller_id_type/2 :: (Resource :: json_object(), Call :: #cf_call{}) -> raw | binary()).
get_caller_id_type(Resource, #cf_call{channel_vars=CVs}) ->
    case whistle_util:is_true(wh_json:get_value(<<"CF-Keep-Caller-ID">>, CVs)) of
        false -> wh_json:get_value([<<"value">>, <<"caller_id_options">>, <<"type">>], Resource, <<"external">>);
        true -> raw
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function recieves a resource rule (regex) and determines if
%% the destination number matches.  If it does and the regex has a
%% capture group return the group, if not but it matched return the
%% full destination number otherwise return an empty list.
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
