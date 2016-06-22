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
find_numbers(Number, Quantity, Options) ->
    case props:get_value(?KNM_ACCOUNTID_CARRIER, Options) of
        'undefined' -> {'error', 'not_available'};
        AccountId ->
            ResellerId = kz_services:find_reseller_id(AccountId),
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            case ResellerId == MasterAccountId of
                'false' -> {'error', 'not_available'};
                'true' ->
                    do_find_numbers(Number, Quantity, AccountId)
                    %% TODO: given the requestor's account, discover knm_local numbers
                    %%        that are available but managed by accendants of the account.
            end
    end.

-spec do_find_numbers(ne_binary(), pos_integer(), ne_binary()) ->
                             {'ok', knm_number:knm_numbers()} |
                             {'error', any()}.
do_find_numbers(<<"+",_/binary>>=Number, Quantity, AccountId)
  when is_integer(Quantity), Quantity > 0 ->
    ViewOptions = [{'startkey', [?NUMBER_STATE_AVAILABLE, Number]}
                  ,{'endkey', [?NUMBER_STATE_AVAILABLE, <<"\ufff0">>]}
                  ,{'limit', Quantity}
                  ],
    case
        'undefined' /= (DB = knm_converters:to_db(Number)) andalso
        kz_datamgr:get_results(DB, <<"numbers/status">>, ViewOptions)
    of
        'false' -> {'error', 'not_available'};
        {'ok', []} ->
            lager:debug("found no available local numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found available local numbers for account ~s", [AccountId]),
            Numbers = format_numbers(JObjs),
            find_more(Quantity, AccountId, length(Numbers), Numbers);
        {'error', _R}=E ->
            lager:debug("failed to lookup available local numbers: ~p", [_R]),
            E
    end;
do_find_numbers(_, _, _) ->
    {'error', 'not_available'}.

-spec find_more(pos_integer(), ne_binary(), pos_integer(), knm_number:knm_numbers()) ->
                       knm_number:knm_numbers().
find_more(Quantity, AccountId, NotEnough, Numbers)
  when NotEnough < Quantity ->
    NewStart = bump(knm_phone_number:number(knm_number:phone_number(lists:last(Numbers)))),
    case do_find_numbers(NewStart, Quantity - NotEnough, AccountId) of
        {'ok', MoreNumbers} -> {'ok', Numbers ++ MoreNumbers};
        _Error -> {'ok', Numbers}
    end;
find_more(_, _, _Enough, Numbers) ->
    {'ok', Numbers}.

-spec format_numbers(kz_json:objects()) -> knm_number:knm_numbers().
format_numbers(JObjs) ->
    Nums = [kz_doc:id(JObj) || JObj <- JObjs],
    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
              ],
    [Number || {_Num,{'ok',Number}} <- knm_numbers:get(Nums, Options)].

-spec bump(ne_binary()) -> ne_binary().
bump(<<"+", Num/binary>>) ->
    Bumped = erlang:integer_to_binary(1 + erlang:binary_to_integer(Num)),
    <<"+", Bumped/binary>>.

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
