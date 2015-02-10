%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number at Simwood (UK based provider)
%%% https://www.simwood.com/services/api
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(wnm_simwood).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
         ,is_number_billable/1
         ,should_lookup_cnam/0
        ]).

-include("../wnm.hrl").

-define(WNM_SW_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".simwood">>).

-define(SW_NUMBER_URL, whapps_config:get_string(?WNM_SW_CONFIG_CAT
                                                ,<<"numbers_api_url">>
                                                ,<<"https://api.simwood.com/v3/numbers">>)).

-define(SW_ACCOUNT_ID, whapps_config:get_string(?WNM_SW_CONFIG_CAT, <<"simwood_account_id">>, <<>>)).
-define(SW_AUTH_USERNAME, whapps_config:get_string(?WNM_SW_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(SW_AUTH_PASSWORD, whapps_config:get_string(?WNM_SW_CONFIG_CAT, <<"auth_password">>, <<>>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query Simwood.com for available numbers
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
find_numbers(Prefix, Quantity, _Options) ->
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/available/standard/">>, sw_quantity(Quantity), "?pattern=", Prefix, "*"]),
    {'ok', Body} = query_simwood(URL, 'get'),
    process_response(wh_json:decode(Body)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from Simwood.com
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{dry_run='true'}=NR) -> NR;
acquire_number(#number{number=(<<$+, Number/binary>>)}=NR) ->
    acquire_number(NR#number{number=Number});
acquire_number(#number{number=Number}=NR) ->
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, wh_util:to_binary(Number)]),
    {'ok', _Body} = query_simwood(URL, 'put'),
    NR#number{number=(wnm_util:normalize_number(Number))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return number back to Simwood.com
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(#number{number=(<<$+, Number/binary>>)}=NR) ->
    disconnect_number(NR#number{number=Number});
disconnect_number(#number{number=Number}=NR) ->
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, wh_util:to_binary(Number)]),
    {'ok', _Body} = query_simwood(URL, 'delete'),
    NR#number{number=(wnm_util:normalize_number(Number))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec is_number_billable(wnm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec query_simwood(ne_binary(), 'get'|'put'|'post'|'delete') ->
                           {'ok', iolist()} |
                           {'error', 'not_available'}.
query_simwood(URL, Verb) ->
    lager:debug("Querying Simwood. Verb: ~p. URL: ~p.", [Verb, URL]),
    HTTPOptions = [{'ssl',[{'verify',0}]}
                   ,{'inactivity_timeout', 180000}
                   ,{'connect_timeout', 180000}
                   ,{'basic_auth', {?SW_AUTH_USERNAME, ?SW_AUTH_PASSWORD}}
                  ],
    case ibrowse:send_req(wh_util:to_list(URL), [], Verb, [], HTTPOptions) of
        {'ok', _Resp, _RespHeaders, Body} ->
            lager:debug("Simwood response ~p: ~p", [_Resp, Body]),
            {'ok', Body};
        {'error', _R} ->
            lager:debug("Simwood response: ~p", [_R]),
            {'error', 'not_available'}
    end.

%%
%%  Simwood number query supports only 1|10|100 search amount
%%
-spec sw_quantity(pos_integer()) -> ne_binary().
sw_quantity(Quantity) when Quantity == 1 -> <<"1">>;
sw_quantity(Quantity) when Quantity > 1, Quantity =< 10  -> <<"10">>;
sw_quantity(_Quantity) -> <<"100">>.

-spec process_response(wh_json:objects()) -> {'ok', wh_json:object()}.
process_response([]) -> {'ok', wh_json:new()};
process_response(JObjs) ->
    process_response(JObjs, wh_json:new()).

-spec process_response(wh_json:objects(), wh_json:object()) -> {'ok', wh_json:object()}.
process_response([], Acc) -> {'ok', Acc};
process_response([JObj|JObjs], Acc) ->
    CountryCode = wh_json:get_value(<<"country_code">>, JObj),
    FoundNumber = wh_json:get_value(<<"number">>, JObj),
    E164 = <<"+", CountryCode/binary, FoundNumber/binary>>,
    NumberJObj = wh_json:from_list([{<<"number">>, E164}]),
    process_response(JObjs, wh_json:set_value(E164, NumberJObj, Acc)).
