%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
-export([get_sip_headers/1]).

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
    case whapps_config:get_is_true(?SS_CONFIG_CAT, <<"assume_inbound_e164">>, 'false') of
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
                           {'ok', ne_binary(), number_properties()} |
                           {'error', any()}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, cache_key_number(Num)) of
        {'ok', {AccountId, Props}} ->
            lager:debug("found number properties in stepswitch cache"),
            {'ok', AccountId, Props};
        {'error', 'not_found'} -> fetch_number(Num)
    end.

-spec fetch_number(ne_binary()) ->
                          {'ok', ne_binary(), number_properties()} |
                          {'error', any()}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {'ok', AccountId, Props} ->
            CacheProps = [{'origin', [{'db', wnm_util:number_to_db_name(Num), Num}, {'type', <<"number">>}]}],
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
-spec correct_shortdial(ne_binary(), ne_binary() | wh_json:object()) -> api_binary().
correct_shortdial(<<"+", Number/binary>>, CIDNum) ->
    correct_shortdial(Number, CIDNum);
correct_shortdial(Number, <<"+", CIDNum/binary>>) ->
    correct_shortdial(Number, CIDNum);
correct_shortdial(Number, CIDNum) when is_binary(CIDNum) ->
    MaxCorrection = whapps_config:get_integer(?SS_CONFIG_CAT, <<"max_shortdial_correction">>, 5),
    MinCorrection = whapps_config:get_integer(?SS_CONFIG_CAT, <<"min_shortdial_correction">>, 2),
    case is_binary(CIDNum) andalso (size(CIDNum) - size(Number)) of
        Length when Length =< MaxCorrection, Length >= MinCorrection ->
            Correction = binary:part(CIDNum, 0, Length),
            CorrectedNumber = wnm_util:to_e164(<<Correction/binary, Number/binary>>),
            lager:debug("corrected shortdial ~s via CID ~s to ~s"
                       ,[Number, CIDNum, CorrectedNumber]),
            CorrectedNumber;
        _ ->
            lager:debug("unable to correct shortdial ~s via CID ~s"
                        ,[Number, CIDNum]),
            'undefined'
    end;
correct_shortdial(Number, JObj) ->
    CIDNum = wh_json:get_first_defined(
               [<<"Outbound-Caller-ID-Number">>
                ,<<"Emergency-Caller-ID-Number">>
               ]
               ,JObj
               ,Number
              ),
    correct_shortdial(Number, CIDNum).

-spec get_sip_headers(wh_json:object()) -> api_object().
get_sip_headers(JObj) ->
    SIPHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, JObj, wh_json:new()),
    case get_diversions(SIPHeaders) of
        'undefined' ->
            maybe_remove_diversions(SIPHeaders);
        Diversions ->
            lager:debug("setting diversions ~p", [Diversions]),
            wh_json:set_value(<<"Diversions">>
                              ,Diversions
                              ,SIPHeaders
                             )
    end.

-spec maybe_remove_diversions(api_object()) -> api_object().
maybe_remove_diversions('undefined') -> 'undefined';
maybe_remove_diversions(JObj) ->
    wh_json:delete_key(<<"Diversions">>, JObj).

-spec get_diversions(wh_json:object()) ->
                            'undefined' |
                            ne_binaries().
-spec get_diversions(api_binary(), ne_binaries()) ->
                            'undefined' |
                            ne_binaries().

get_diversions(JObj) ->
    Inception = wh_json:get_value(<<"Inception">>, JObj),
    Diversions = wh_json:get_value(<<"Diversions">>, JObj, []),
    get_diversions(Inception, Diversions).

get_diversions('undefined', _Diversion) -> 'undefined';
get_diversions(_Inception, []) -> 'undefined';
get_diversions(Inception, Diversions) ->
    Fs = [{fun kzsip_diversion:set_address/2, <<"sip:", Inception/binary>>}
          ,{fun kzsip_diversion:set_counter/2, find_diversion_count(Diversions) + 1}
         ],
    [kzsip_diversion:to_binary(
       lists:foldl(fun({F, V}, D) -> F(D, V) end
                   ,kzsip_diversion:new()
                   ,Fs
                  )
      )
    ].

-spec find_diversion_count(ne_binaries()) -> non_neg_integer().
find_diversion_count(Diversions) ->
    lists:max([kzsip_diversion:counter(
                 kzsip_diversion:from_binary(Diversion)
                )
               || Diversion <- Diversions
              ]).
