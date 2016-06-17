%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Find reserved numbers in a sub-account.
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_reserved).
-behaviour(knm_gen_carrier).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of reserved numbers
%% assigned to an account's reseller account.
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), kz_proplist()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(<<"+",_/binary>>=Number, Quantity, Options) ->
    case props:get_value(?KNM_ACCOUNTID_CARRIER, Options) of
        'undefined' -> {'error', 'not_available'};
        AccountId ->
            do_find_numbers(Number, Quantity, AccountId)
    end.

-spec do_find_numbers(ne_binary(), pos_integer(), ne_binary()) ->
                             {'ok', knm_number:knm_numbers()} |
                             {'error', any()}.
do_find_numbers(Number, Quantity, AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = [{'startkey', Number}
                  ,{'endkey', <<Number/binary, "\ufff0">>}
                  ,{'limit', Quantity}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"numbers/list_reserved">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no reserved numbers found"),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found reserved numbers in ~s", [AccountDb]),
            {'ok', format_numbers(AccountId, JObjs)};
        {'error', _R}=E ->
            lager:debug("failed to lookup reserved numbers in ~s: ~p", [AccountDb, _R]),
            E
    end.

-spec format_numbers(ne_binary(), kz_json:objects()) -> knm_number:knm_numbers().
format_numbers(AuthBy, JObjs) ->
    Nums = [kz_doc:id(JObj) || JObj <- JObjs],
    Options = [{'auth_by', AuthBy}
              ],
    [Number || {_Num,{'ok',Number}} <- knm_numbers:get(Nums, Options)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_number:knm_number()) -> 'false'.
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------

-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
