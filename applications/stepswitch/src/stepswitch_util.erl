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
        {'ok', {AccountId, Props}} -> 
            lager:debug("found number properties in stepswitch cache"),
            {'ok', AccountId, Props};
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
            CorrectedNumber = wnm_util:to_e164(<<(binary:part(CIDNum, 0, Length))/binary, Number/binary>>),
            lager:debug("corrected shortdial ~s via CID ~s to ~s"
                        ,[Number, CIDNum, CorrectedNumber]),
            CorrectedNumber;
        _ ->
            lager:debug("unable to correct shortdial ~s via CID ~s"
                        ,[Number, CIDNum]),
            'undefined'
    end.
