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

-define(VIEW_BY_RULES, <<"cf_attributes/active_resources_by_rules">>).

-type endpoint() :: {binary(), wh_json:json_objects(), 'raw' | binary()}.
-type endpoints() :: [] | [endpoint()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> ok.
handle(Data, Call) ->
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
-spec bridge_to_resources/5 :: (endpoints(), cf_api_binary(), cf_api_binary(), cf_api_binary(), #cf_call{}) -> ok.
bridge_to_resources([{DestNum, Rsc, _CIDType}|T], Timeout, IgnoreEarlyMedia, Ringback, #cf_call{account_id=AccountId}=Call) ->
    Endpoint = [create_endpoint(DestNum, Gtw) || Gtw <- wh_json:get_value(<<"gateways">>, Rsc)],
    case cf_call_command:b_bridge(Endpoint, Timeout, <<"single">>, IgnoreEarlyMedia, Ringback, Call) of
        {ok, _} ->
            ?LOG("completed successful bridge to resource"),
            cf_exe:stop(Call);
        {fail, R} when T =:= [] ->
            {Cause, Code} = whapps_util:get_call_termination_reason(R),
            ?LOG(notice, "exhausted all local resources attempting bridge, final cause ~s:~s"
                 ,[Code, Cause, {extra_data, [{details, cf_util:call_to_proplist(Call)}
                                              ,{account_id, AccountId}
                                             ]}]),
            case (cf_util:handle_bridge_failure(Cause, Call) =:= ok)
                orelse (cf_util:handle_bridge_failure(Code, Call) =:= ok) of
                true -> ok;
                false ->
                    cf_util:send_default_response(Cause, Call),
                    cf_exe:continue(Call)
            end;
        {fail, _} ->
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Call);
        {error, _R} ->
            ?LOG(notice, "error attemping local resource to ~s: ~p", [DestNum, _R, {extra_data, [{details, cf_util:call_to_proplist(Call)}
                                                                                                 ,{account_id, AccountId}
                                                                                                ]}]),
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Call)
    end;
bridge_to_resources([], _, _, _, Call) ->
    ?LOG("resources exhausted without success"),
    case cf_util:handle_bridge_failure(<<"NO_ROUTE_DESTINATION">>, Call) =:= ok of
        true -> ok;
        false ->
            cf_util:send_default_response(<<"NO_ROUTE_DESTINATION">>, Call),
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This will build endpoint from the giving resource/rule combination
%% for use with the whistle bridge API.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoint/2 :: (ne_binary(), wh_json:json_object()) -> wh_json:json_object().
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
    wh_json:from_list([ KV || {_, V}=KV <- Endpoint, V =/= undefined ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve a complete list of resources in this database that are
%% enabled.  Remove any where the rules do not apply, and the destination
%% number as formated by that rule (ie: capture group or full number).
%% @end
%%--------------------------------------------------------------------
-spec find_endpoints/1 :: (#cf_call{}) -> {'ok', endpoints()} | {'error', atom()}.
find_endpoints(#cf_call{account_db=Db, request_user=ReqNum}=Call) ->
    ?LOG("searching for resource endpoints"),
    case couch_mgr:get_results(Db, ?VIEW_BY_RULES, []) of
        {ok, Resources} ->
            ?LOG("found resources, filtering by rules"),
            {ok, [{Number
                   ,wh_json:get_value([<<"value">>], Resource, [])
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
-spec(get_caller_id_type/2 :: (Resource :: wh_json:json_object(), Call :: #cf_call{}) -> raw | binary()).
get_caller_id_type(Resource, #cf_call{channel_vars=CVs}) ->
    case wh_json:is_true(<<"emergency">>, Resource) of
        true ->
            <<"emergency">>;
        false ->
            Type = wh_json:get_value([<<"value">>, <<"caller_id_options">>, <<"type">>], Resource, <<"external">>),
            case wh_json:is_true(<<"CF-Keep-Caller-ID">>, CVs) andalso (Type =/= <<"emergency">>) of
                false -> Type;
                true -> raw
            end
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
-spec evaluate_rules/2 :: (list(), ne_binary()) -> [] | [ne_binary(),...].
evaluate_rules([_, Regex], DestNum) ->
    case re:run(DestNum, Regex) of
        {match, [_, {Start,End}|_]} ->
            [binary:part(DestNum, Start, End)];
        {match, _} ->
            [DestNum];
        _ -> []
    end.
