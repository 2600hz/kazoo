%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number at Simwood (UK based provider)
%%% https://www.simwood.com/services/api
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_simwood).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

-define(KNM_SW_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".simwood">>).

-define(SW_NUMBER_URL
       ,kapps_config:get_string(?KNM_SW_CONFIG_CAT
                               ,<<"numbers_api_url">>
                               ,<<"https://api.simwood.com/v3/numbers">>
                               )
       ).

-define(SW_ACCOUNT_ID, kapps_config:get_string(?KNM_SW_CONFIG_CAT, <<"simwood_account_id">>, <<>>)).
-define(SW_AUTH_USERNAME, kapps_config:get_binary(?KNM_SW_CONFIG_CAT, <<"auth_username">>, <<>>)).
-define(SW_AUTH_PASSWORD, kapps_config:get_binary(?KNM_SW_CONFIG_CAT, <<"auth_password">>, <<>>)).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check with carrier if these numbers are registered with it.
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {ok, kz_json:object()} |
                                              {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query Simwood.com for available numbers
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()}.
find_numbers(Prefix, Quantity, Options) ->
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/available/standard/">>, sw_quantity(Quantity), "?pattern=", Prefix, "*"]),
    {'ok', Body} = query_simwood(URL, 'get'),
    process_response(kz_json:decode(Body), Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from Simwood.com
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) ->
    Num = to_simwood(Number),
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, Num]),
    case query_simwood(URL, 'put') of
        {'ok', _Body} -> Number;
        {'error', Error} ->
            knm_errors:by_carrier(?MODULE, Error, Num)
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
    Num = to_simwood(Number),
    URL = list_to_binary([?SW_NUMBER_URL, "/", ?SW_ACCOUNT_ID, <<"/allocated/">>, Num]),
    case query_simwood(URL, 'delete') of
        {'ok', _Body} -> Number;
        {'error', Error} ->
            knm_errors:by_carrier(?MODULE, Error, Num)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
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
-spec to_simwood(knm_number:knm_number()) -> kz_term:ne_binary().
to_simwood(Number) ->
    case knm_phone_number:number(knm_number:phone_number(Number)) of
        <<$+, N/binary>> -> N;
        N -> N
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec query_simwood(kz_term:ne_binary(), 'get' | 'put' | 'delete') ->
                           {'ok', iolist()} |
                           {'error', 'not_available'}.
query_simwood(URL, Verb) ->
    lager:debug("Querying Simwood. Verb: ~p. URL: ~p.", [Verb, URL]),
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'basic_auth', {?SW_AUTH_USERNAME, ?SW_AUTH_PASSWORD}}
                  ],
    case kz_http:req(Verb, kz_term:to_binary(URL), [], [], HTTPOptions) of
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
-spec sw_quantity(pos_integer()) -> kz_term:ne_binary().
sw_quantity(Quantity) when Quantity == 1 -> <<"1">>;
sw_quantity(Quantity) when Quantity > 1, Quantity =< 10 -> <<"10">>;
sw_quantity(_Quantity) -> <<"100">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec process_response(kz_json:objects(), knm_carriers:options()) ->
                              {'ok', knm_number:knm_numbers()}.
process_response(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    {'ok', [N || JObj <- JObjs,
                 N <- [response_jobj_to_number(JObj, QID)]
           ]}.

response_jobj_to_number(JObj, QID) ->
    Num = <<(kz_json:get_binary_value(<<"country_code">>, JObj))/binary
            ,(kz_json:get_binary_value(<<"number">>, JObj))/binary
          >>,
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.
