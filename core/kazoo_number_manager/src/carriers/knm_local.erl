%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Find manually-added `available' numbers.
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_local).
-behaviour(knm_gen_carrier).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

-define(PREFER_AVAILABLE_NUMBERS,
        kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"search_prefers_available_numbers">>, 'false')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), kz_proplist()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(Number, Quantity, Options) when size(Number) < 5 ->
    find_numbers(<<"+1", Number/binary>>, Quantity, Options);
find_numbers(Number, Quantity, Options) ->
    case ?PREFER_AVAILABLE_NUMBERS of
        'false' -> {'error', 'not_available'};
        'true' ->
            AccountId = props:get_value(<<"account_id">>, Options),
            do_find_numbers(Number, Quantity, AccountId)
            %% TODO: given the requestor's account, discover knm_local numbers
            %%        that are available but managed by accendants of the account.
    end.

-spec do_find_numbers(ne_binary(), pos_integer(), ne_binary()) ->
                             {'ok', knm_number:knm_numbers()} |
                             {'error', any()}.
do_find_numbers(Number, Quantity, AccountId) ->
    ViewOptions = [{'startkey', [?NUMBER_STATE_AVAILABLE, Number]}
                  ,{'endkey', [?NUMBER_STATE_AVAILABLE, <<"\ufff0">>]}
                  ,{'limit', Quantity}
                  ],
    DB = knm_converters:to_db(Number),
    case kz_datamgr:get_results(DB, <<"numbers/status">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("found no available local numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found ~p available local numbers for account ~s", [length(JObjs), AccountId]),
            {'ok', format_numbers(AccountId, JObjs)};
        {'error', _R}=E ->
            lager:debug("failed to lookup available local numbers: ~p", [_R]),
            E
    end.

-spec format_numbers(ne_binary(), kz_json:objects()) -> knm_number:knm_numbers().
format_numbers(AccountId, JObjs) ->
    Nums = [kz_doc:id(JObj) || JObj <- JObjs],
    Options = [{'auth_by', AccountId}
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
