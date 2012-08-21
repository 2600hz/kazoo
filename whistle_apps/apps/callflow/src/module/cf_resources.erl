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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {ok, Endpoints} = find_endpoints(Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, <<"60">>),
    IgnoreEarlyMedia = wh_json:get_value(<<"ignore_early_media">>, Data),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    bridge_to_resources(Endpoints, Timeout, IgnoreEarlyMedia, Ringback, Data, Call).

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
-spec bridge_to_resources/6 :: (endpoints(), cf_api_binary(), cf_api_binary(), cf_api_binary(), wh_json:json_object(), whapps_call:call()) -> 'ok'.
bridge_to_resources([{DestNum, Rsc, _CIDType}|T], Timeout, IgnoreEarlyMedia, Ringback, Data, Call) ->
    Endpoint = [create_endpoint(DestNum, Gtw, Call)
                || Gtw <- wh_json:get_value(<<"gateways">>, Rsc)
               ],
    SIPHeaders = build_sip_headers(Data, Call),
    case whapps_call_command:b_bridge(Endpoint, Timeout, <<"single">>, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) of
        {ok, _} ->
            lager:debug("completed successful bridge to resource"),
            cf_exe:stop(Call);
        {fail, R} when T =:= [] ->
            {Cause, Code} = whapps_util:get_call_termination_reason(R),
            lager:notice("exhausted all local resources attempting bridge, final cause ~s:~s", [Code, Cause]),
            case (cf_util:handle_bridge_failure(Cause, Call) =:= ok)
                orelse (cf_util:handle_bridge_failure(Code, Call) =:= ok) of
                true -> ok;
                false ->
                    cf_util:send_default_response(Cause, Call),
                    cf_exe:continue(Call)
            end;
        {fail, _} ->
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Data, Call);
        {error, _R} ->
            lager:notice("error attemping local resource to ~s: ~p", [DestNum, _R]),
            bridge_to_resources(T, Timeout, IgnoreEarlyMedia, Ringback, Data, Call)
    end;
bridge_to_resources([], _, _, _, _, Call) ->
    lager:debug("resources exhausted without success"),
    WildcardIsEmpty = cf_exe:wildcard_is_empty(Call),
    case cf_util:handle_bridge_failure(<<"NO_ROUTE_DESTINATION">>, Call) =:= ok of
        true -> ok;
        false when WildcardIsEmpty ->
            cf_util:send_default_response(<<"NO_ROUTE_DESTINATION">>, Call),
            cf_exe:continue(Call);
        false -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This will build endpoint from the giving resource/rule combination
%% for use with the whistle bridge API.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoint/3 :: (ne_binary(), wh_json:json_object(), whapps_call:call()) -> wh_json:json_object().
create_endpoint(DestNum, JObj, Call) ->
    AccountDb = whapps_call:account_db(Call), 
    CNum = whapps_call:caller_id_number(Call),
    Rule = <<"sip:"
              ,(wh_json:get_value(<<"prefix">>, JObj, <<>>))/binary
              ,DestNum/binary
              ,(wh_json:get_value(<<"suffix">>, JObj, <<>>))/binary
              ,$@ ,(wh_json:get_value(<<"server">>, JObj))/binary>>,
    lager:debug("attempting resource ~s", [Rule]),
    AccountId = whapps_call:account_id(Call),
    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
            ,{<<"From-URI">>, maybe_from_uri(AccountDb, JObj, CNum)}
            ,{<<"Ignore-Display-Updates">>, <<"true">>}
            ,{<<"Global-Resource">>, false}
           ],
    Endpoint = [{<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Rule}
                ,{<<"Auth-User">>, wh_json:get_value(<<"username">>, JObj)}
                ,{<<"Auth-Password">>, wh_json:get_value(<<"password">>, JObj)}
                ,{<<"Bypass-Media">>, wh_json:get_value(<<"bypass_media">>, JObj)}
                ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, JObj, <<"6">>)}
                ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, JObj)}
                ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:filter_undefined(CCVs))}
               ],
    wh_json:from_list(props:filter_undefined(Endpoint)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve a complete list of resources in this database that are
%% enabled.  Remove any where the rules do not apply, and the destination
%% number as formated by that rule (ie: capture group or full number).
%% @end
%%--------------------------------------------------------------------
-spec find_endpoints/1 :: (whapps_call:call()) -> {'ok', endpoints()} | {'error', atom()}.
find_endpoints(Call) ->
    lager:debug("searching for resource endpoints"),
    AccountDb = whapps_call:account_db(Call),

    case couch_mgr:get_results(AccountDb, ?VIEW_BY_RULES, []) of
        {ok, Resources} ->
            lager:debug("found resources, filtering by rules"),
            {ok, [{Number
                   ,wh_json:get_value([<<"value">>], Resource, [])
                   ,get_caller_id_type(Resource, Call)}
                  || Resource <- Resources
                         ,Number <- evaluate_rules(wh_json:get_value(<<"key">>, Resource), whapps_call:request_user(Call))
                         ,Number =/= []
                 ]};
        {error, R}=E ->
            lager:debug("search failed ~w", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_caller_id_type/2 :: (wh_json:json_object(), whapps_call:call()) -> 'raw' | binary().
get_caller_id_type(Resource, Call) ->
    case wh_json:is_true(<<"emergency">>, Resource) of
        true ->
            <<"emergency">>;
        false ->
            CCVs = whapps_call:custom_channel_vars(Call),
            Type = wh_json:get_value([<<"value">>, <<"caller_id_options">>, <<"type">>], Resource, <<"external">>),
            case wh_json:is_true(<<"CF-Keep-Caller-ID">>, CCVs) andalso (Type =/= <<"emergency">>) of
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

maybe_from_uri(true, CNum, Realm) ->
    from_uri(CNum, Realm);
maybe_from_uri(false, CNum, Realm) ->
    case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>) of
        true -> from_uri(CNum, Realm);
        false -> undefined;
        undefined -> undefined
    end;
maybe_from_uri(AccountDb, Gateway, CNum) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),

    {ok, AcctDoc} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    Realm = wh_json:get_value(<<"realm">>, AcctDoc),
    case wh_json:is_true(<<"format_from_uri">>, Gateway, false) of
        true -> from_uri(CNum, Realm);
        false -> maybe_from_uri(wh_json:is_true(<<"format_from_uri">>, AcctDoc, false), CNum, Realm)
    end.

from_uri(?NE_BINARY = CNum, Realm) ->
    lager:debug("setting From-URI to sip:~s@~s", [CNum, Realm]),
    <<"sip:", CNum/binary, "@", Realm/binary>>;
from_uri(_, _) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the callflow data object for this instance of the local resources
%% defines custom sip headers or flags to include custom sip headers
%% build a json object of those now.
%% @end
%%--------------------------------------------------------------------
-spec build_sip_headers/2 :: (wh_json:json_object(), whapps_call:call()) -> 'undefined' | wh_json:json_object().
build_sip_headers(Data, Call) ->
    Builders = [fun(J) ->
                        case wh_json:is_true(<<"emit_account_id">>, Data) of
                            false -> J;
                            true -> 
                                wh_json:set_value(<<"X-Account-ID">>, whapps_call:account_id(Call), J)
                        end
                end
               ],
    CustomHeaders = wh_json:get_value(<<"custom_sip_headers">>, Data, wh_json:new()),
    JObj = lists:foldl(fun(F, J) -> F(J) end, CustomHeaders, Builders),
    case wh_util:is_empty(JObj) of
        true -> undefined;
        false -> JObj
    end.
