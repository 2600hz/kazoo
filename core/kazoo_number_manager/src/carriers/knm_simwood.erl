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
-module(knm_simwood).

-behaviour(knm_gen_carrier).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
         ,is_number_billable/1
         ,should_lookup_cnam/0
        ]).

-include("knm.hrl").

-define(KNM_SW_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".simwood">>).

-define(SW_NUMBER_URL
        ,whapps_config:get_string(?KNM_SW_CONFIG_CAT
                                  ,<<"numbers_api_url">>
                                  ,<<"https://api.simwood.com/v3/numbers">>
                                 )
       ).

-define(SW_ACCOUNT_ID, whapps_config:get_string(?KNM_SW_CONFIG_CAT, <<"simwood_account_id">>, <<>>)).
-define(SW_AUTH_USERNAME, whapps_config:get_binary(?KNM_SW_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(SW_AUTH_PASSWORD, whapps_config:get_binary(?KNM_SW_CONFIG_CAT, <<"auth_password">>, <<>>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query Simwood.com for available numbers
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', knm_number:knm_numbers()}.
find_numbers(Prefix, Quantity, Options) ->
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/available/standard/">>, sw_quantity(Quantity), "?pattern=", Prefix, "*"]),
    {'ok', Body} = query_simwood(URL, 'get'),
    process_response(wh_json:decode(Body), Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from Simwood.com
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num =
        case knm_phone_number:number(PhoneNumber) of
            <<$+, N/binary>> -> N;
            N -> N
        end,
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, wh_util:to_binary(Num)]),
    case query_simwood(URL, 'put') of
        {'ok', _Body} -> Number;
        {'error', Error} -> knm_errors:unspecified(Error, Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return number back to Simwood.com
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num =
        case knm_phone_number:number(PhoneNumber) of
            <<$+, N/binary>> -> N;
            N -> N
        end,
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, wh_util:to_binary(Num)]),
    case query_simwood(URL, 'delete') of
        {'ok', _Body} -> Number;
        {'error', Error} -> knm_errors:unspecified(Error, Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_number:knm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec query_simwood(ne_binary(), 'get'|'put'|'delete') ->
                           {'ok', iolist()} |
                           {'error', 'not_available'}.
query_simwood(URL, Verb) ->
    lager:debug("Querying Simwood. Verb: ~p. URL: ~p.", [Verb, URL]),
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}]}
                   ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'basic_auth', {?SW_AUTH_USERNAME, ?SW_AUTH_PASSWORD}}
                  ],
    case kz_http:req(Verb, wh_util:to_binary(URL), [], [], HTTPOptions) of
        {'ok', _Resp, _RespHeaders, Body} ->
            lager:debug("Simwood response ~p: ~p", [_Resp, Body]),
            {'ok', Body};
        {'error', _R} ->
            lager:debug("Simwood response: ~p", [_R]),
            {'error', 'not_available'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Simwood number query supports only 1|10|100 search amount
%% @end
%%--------------------------------------------------------------------
-spec sw_quantity(pos_integer()) -> ne_binary().
sw_quantity(Quantity) when Quantity == 1 -> <<"1">>;
sw_quantity(Quantity) when Quantity > 1, Quantity =< 10  -> <<"10">>;
sw_quantity(_Quantity) -> <<"100">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec process_response(wh_json:objects(), wh_proplist()) ->
                              {'ok', knm_number:knm_numbers()}.
process_response(JObjs, Options) ->
    AccountId = props:get_value(<<"account_id">>, Options),
    {'ok', [response_jobj_to_number(JObj, AccountId) || JObj <- JObjs]}.

-spec response_jobj_to_number(wh_json:object(), api_binary()) ->
                              knm_number:knm_number().
response_jobj_to_number(JObj, AccountId) ->
    Num = wh_json:get_value(<<"number">>, JObj),
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    Updates = [{fun knm_phone_number:set_number/2, NormalizedNum}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
               ,{fun knm_phone_number:set_carrier_data/2, JObj}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_assign_to/2, AccountId}
              ],
    {'ok', PhoneNumber} = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_number:set_phone_number(knm_number:new(), PhoneNumber).
