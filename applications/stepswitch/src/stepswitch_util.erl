%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_util).

-export([get_realm/1]).
-export([get_inbound_destination/1]).
-export([get_outbound_destination/1]).
-export([lookup_number/1]).
-export([correct_shortdial/2]).
-export([hangup_result/1]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_realm(api_binary() | wh_json:object()) -> api_binary().
get_realm('undefined') -> 'undefined';
get_realm(From) when is_binary(From) ->
    case binary:split(From, <<"@">>) of
        [_, Realm] -> Realm;
        _Else -> 'undefined'
    end;
get_realm(JObj) ->
    AuthRealm = wh_json:get_value(<<"Auth-Realm">>, JObj),
    case wh_util:is_empty(AuthRealm)
        orelse wh_network_utils:is_ipv4(AuthRealm) 
        orelse wh_network_utils:is_ipv6(AuthRealm) 
    of
        'false' -> AuthRealm;
        'true' ->
            get_realm(wh_json:get_value(<<"From">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_inbound_destination(wh_json:object()) -> ne_binary().
get_inbound_destination(JObj) ->
    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case whapps_config:get_is_true(<<"stepswitch">>, <<"assume_inbound_e164">>, 'false') of
        'true' -> assume_e164(Number);
        'false' -> wnm_util:to_e164(Number)
    end.

-spec assume_e164(ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_destination(wh_json:object()) -> ne_binary().
get_outbound_destination(JObj) ->
    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
     case wh_json:is_true(<<"Bypass-E164">>, JObj) of
         'false' -> wnm_util:to_e164(Number);
         'true' -> Number
     end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(ne_binary()) ->
                           {'ok', ne_binary(), wh_proplist()} |
                           {'error', term()}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, cache_key_number(Num)) of
        {'ok', {AccountId, Props}} -> {'ok', AccountId, Props};
        {'error', 'not_found'} -> fetch_number(Num)
    end.

-spec fetch_number(ne_binary()) ->
                          {'ok', ne_binary(), wh_proplist()} |
                          {'error', term()}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {'ok', AccountId, Props} ->
            CacheProps = [{'origin', {'db', wnm_util:number_to_db_name(Num), Num}}],
            wh_cache:store_local(?STEPSWITCH_CACHE, cache_key_number(Num), {AccountId, Props}, CacheProps),
            lager:debug("~s is associated with account ~s", [Num, AccountId]),
            {'ok', AccountId, Props};
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec maybe_transition_port_in(ne_binary(), wh_proplist()) -> 'false' | pid().
maybe_transition_port_in(Num, Props) ->
    case props:get_value('pending_port', Props) of
        'false' -> 'false';
        'true' -> spawn('wh_number_manager', 'ported', [Num])
    end.

-spec cache_key_number(ne_binary()) -> {'stepswitch_number', ne_binary()}.
cache_key_number(Number) ->
    {'stepswitch_number', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% if the given number is shorter then a known caller id then try
%% to pad the front of the dialed number with values from the
%% callerid.
%% @end
%%--------------------------------------------------------------------
-spec correct_shortdial(ne_binary(), wh_json:object()) -> ne_binary() | 'undefined'.
correct_shortdial(Number, JObj) ->
    CIDNum = wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                        ,<<"Emergency-Caller-ID-Number">>
                                       ], JObj),
    MaxCorrection = whapps_config:get_integer(<<"stepswitch">>, <<"max_shortdial_correction">>, 5),
    case is_binary(CIDNum) andalso (size(CIDNum) - size(Number)) of
        Length when Length =< MaxCorrection, Length > 0 ->
            wnm_util:to_e164(<<(binary:part(CIDNum, 0, Length))/binary, Number/binary>>);
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the hangup case indicates a call failure
%% @end
%%--------------------------------------------------------------------
-spec hangup_result(wh_json:object()) -> {'ok' | 'fail', wh_json:object()}.
hangup_result(JObj) ->
    AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                    wh_json:get_value(<<"Hangup-Cause">>, JObj)),
    SuccessfulCause = lists:member(AppResponse, ?SUCCESSFUL_HANGUP_CAUSES),
    case wh_json:get_value(<<"Hangup-Code">>, JObj) of
        <<"sip:", Code/binary>> when SuccessfulCause ->
            try wh_util:to_integer(Code) < 400 of
                'true' -> {'ok', JObj};
                'false' when SuccessfulCause ->
                    {'fail', wh_json:set_value(<<"Application-Response">>, <<"NORMAL_TEMPORARY_FAILURE">>, JObj)};
                'false' -> {'fail', JObj}
            catch
                _:_ when SuccessfulCause -> {'ok', JObj};
                _:_  -> {'fail', JObj}
            end;
        _Else when SuccessfulCause -> {'ok', JObj};
        _Else -> {'fail', JObj}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% given the result of either wait_for_bridge or wait_for_execute_extension
%% create and send a Whistle offnet resource response
%% @end
%%--------------------------------------------------------------------
response({'ok', Resp}, JObj) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'ready', Resp}, JObj) ->
    lager:debug("originate is ready to execute"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Resp)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, Resp)}
     ,{<<"Response-Message">>, <<"READY">>}
     ,{<<"Resource-Response">>, Resp}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'fail', Response}, JObj) ->
    lager:debug("resources for outbound request failed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_value(<<"Application-Response">>, Response,
                                                 wh_json:get_value(<<"Hangup-Cause">>, Response))}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, Response)}
     ,{<<"Resource-Response">>, Response}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', 'no_resources'}, JObj) ->
    lager:debug("no available resources"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NO_ROUTE_DESTINATION">>}
     ,{<<"Response-Code">>, <<"sip:404">>}
     ,{<<"Error-Message">>, <<"no available resources">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', 'timeout'}, JObj) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"bridge request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
response({'error', Error}, JObj) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(Error))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, Error, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
