%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Find reserved numbers in an account's reseller account.
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_reserved_reseller).
-behaviour(knm_gen_carrier).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include_lib("kazoo_number_manager/src/knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'true'.

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
find_numbers(Number, Quantity, Options) ->
    case props:get_value(?KNM_ACCOUNTID_CARRIER, Options) of
        'undefined' -> {'error', 'not_available'};
        AccountId ->
            ResellerId = kz_services:find_reseller_id(AccountId),%TODO: add it in Options
            case do_find_numbers(Number, Quantity, ResellerId) of
                {'ok', Enough}=Ok when length(Enough) >= Quantity -> Ok;
                {'error', _R}=Error -> Error;
                {'ok', NotEnough}=Meh ->
                    {'ok', ResellerJObj} = kz_account:fetch(ResellerId),
                    case kz_account:allow_number_additions(ResellerJObj) of
                        'true' -> throw({'stopping_here', NotEnough});
                        'false' -> Meh
                    end
            end
    end.

-spec do_find_numbers(ne_binary(), pos_integer(), ne_binary()) ->
                             {'ok', knm_number:knm_numbers()} |
                             {'error', any()}.
do_find_numbers(<<"+",_/binary>>=Number, Quantity, ResellerId)
  when is_integer(Quantity), Quantity > 0 ->
    ResellerDb = kz_util:format_account_db(ResellerId),
    ViewOptions = [{'startkey', Number}
                  ,{'endkey', <<Number/binary, "\ufff0">>}
                  ,{'limit', Quantity}
                  ],
    case kz_datamgr:get_results(ResellerDb, <<"numbers/list_reserved">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no reserved numbers found"),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found reserved numbers in ~s", [ResellerDb]),
            Numbers = format_numbers(ResellerId, JObjs),
            find_more(Quantity, ResellerId, length(Numbers), Numbers);
        {'error', _R}=E ->
            lager:debug("failed to lookup reserved numbers: ~p", [_R]),
            E
    end;
do_find_numbers(_, _, _) ->
    {'error', 'not_available'}.

-spec find_more(pos_integer(), ne_binary(), pos_integer(), knm_number:knm_numbers()) ->
                       knm_number:knm_numbers().
find_more(Quantity, ResellerId, NotEnough, Numbers)
  when NotEnough < Quantity ->
    NewStart = bump(knm_phone_number:number(knm_number:phone_number(lists:last(Numbers)))),
    case do_find_numbers(NewStart, Quantity - NotEnough, ResellerId) of
        {'ok', MoreNumbers} -> {'ok', Numbers ++ MoreNumbers};
        _Error -> {'ok', Numbers}
    end;
find_more(_, _, _Enough, Numbers) ->
    {'ok', Numbers}.

-spec format_numbers(ne_binary(), kz_json:objects()) -> knm_number:knm_numbers().
format_numbers(ResellerId, JObjs) ->
    Nums = [kz_doc:id(JObj) || JObj <- JObjs],
    Options = [{'auth_by', ResellerId}
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
