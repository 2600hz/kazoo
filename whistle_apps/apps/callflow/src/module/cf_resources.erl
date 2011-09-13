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

-import(cf_call_command, [b_bridge/7, find_failure_branch/2]).

-define(VIEW_BY_RULES, <<"resources/listing_active_by_rules">>).

-type endpoint() :: tuple(binary(), json_objects(), raw | binary()).
-type endpoints() :: [] | [endpoint()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{call_id=CallId}=Call) ->
    put(callid, CallId),
    {ok, Endpoints} = find_endpoints(Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, <<"60">>),
    IgnoreEarlyMedia = wh_json:get_value(<<"ignore_early_media">>, Data),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    bridge_to_resources(Endpoints, Timeout, IgnoreEarlyMedia, Ringback, Call).

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
-spec(bridge_to_resources/5 :: (Endpoints :: endpoints(), Timeout :: cf_api_binary()
                                ,IngoreEarlyMeida :: cf_api_binary(), Ringback :: cf_api_binary()
                                ,Call :: #cf_call{}) -> no_return()).
bridge_to_resources([{DestNum, Gateways, CIDType}|T], Timeout, IgnoreEarlyMedia, Ringback, #cf_call{cf_pid=CFPid
                                                                                                    ,inception_during_transfer=IDT
                                                                                                    ,channel_vars=CCV}=Call) ->
    case b_bridge([create_endpoint(DestNum, Gtw) || Gtw <- Gateways]
                  ,Timeout, CIDType, <<"single">>, IgnoreEarlyMedia, Ringback, Call) of
        {ok, _} ->
            ?LOG("completed successful bridge to resource"),
            CFPid ! { stop };
        {fail, Reason} when IDT, T =:= [] ->
            case wh_json:get_binary_value(<<"Transfer-Fallback">>, CCV) of
                undefined ->
                    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
                    ?LOG("offnet request during transfer failed ~s:~s", [Cause, Code]),
                    find_failure_branch({Cause, Code}, Call)
                        orelse CFPid ! { continue };
                FallbackId ->
                    Gen = [fun(J) -> wh_json:set_value(<<"module">>, <<"device">>, J) end
                           ,fun(J) -> wh_json:set_value([<<"data">>, <<"id">>], FallbackId, J) end
                           ,fun(J) -> wh_json:set_value([<<"children">>, <<"_">>], ?EMPTY_JSON_OBJECT, J) end],
                    Flow = lists:foldr(fun(Fun, JObj) -> Fun(JObj) end, ?EMPTY_JSON_OBJECT, Gen),
                    CFPid ! {branch, Flow}
            end;
        {fail, Reason} when T =:= [] ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("resource failed ~s:~s", [Cause, Code]),
            find_failure_branch({Cause, Code}, Call)
                orelse bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Call);
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("resource failed ~s:~s", [Code, Cause]),
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Call);
        {error, R} ->
            ?LOG("resource bridge error ~p", [R]),
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Call)
    end;
bridge_to_resources([], _, _, _, #cf_call{cf_pid=CFPid}) ->
    ?LOG("resources exhausted without success"),
    CFPid ! { continue }.

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
    Endpoint = [{<<"Invite-Format">>, <<"route">>}
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
-spec(find_endpoints/1 :: (Call :: #cf_call{}) -> {ok, endpoints()} | tuple(error, atom())).
find_endpoints(#cf_call{account_db=Db, request_user=ReqNum}=Call) ->
    ?LOG("searching for resource endpoints"),
    case couch_mgr:get_results(Db, ?VIEW_BY_RULES, []) of
        {ok, Resources} ->
            ?LOG("found resources, filtering by rules"),
            {ok, [{Number
                   ,wh_json:get_value([<<"value">>, <<"gateways">>], Resource, [])
                   ,get_caller_id_type(Resource, Call)}
                  || Resource <- Resources
			 ,Number <- evaluate_rules(wh_json:get_value(<<"key">>, Resource), ReqNum)
			 ,Number =/= []
                 ]};
        {error, R}=E ->
            ?LOG("search failed ~w", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_caller_id_type/2 :: (Resource :: json_object(), Call :: #cf_call{}) -> raw | binary()).
get_caller_id_type(Resource, #cf_call{channel_vars=CVs}) ->
    case wh_util:is_true(wh_json:get_value(<<"CF-Keep-Caller-ID">>, CVs)) of
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
-spec(evaluate_rules/2 :: (Key :: list(), DestNum:: binary()) -> [] | [binary()]).
evaluate_rules([_, Regex], DestNum) ->
    case re:run(DestNum, Regex) of
        {match, [_, {Start,End}|_]} ->
            [binary:part(DestNum, Start, End)];
        {match, _} ->
            [DestNum];
        _ -> []
    end.
