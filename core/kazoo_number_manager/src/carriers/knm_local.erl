%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Allow resellers directly below the master account to find
%%  manually-added `available' numbers.
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_local).
-behaviour(knm_gen_carrier).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

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
%% Check with carrier if these numbers are registered with it.
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries()) -> {ok, kz_json:object()} |
                                      {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_search:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(Prefix, Quantity, Options) ->
    case knm_carriers:account_id(Options) of
        'undefined' -> {'error', 'not_available'};
        AccountId ->
            Offset = knm_carriers:offset(Options),
            QID = knm_search:query_id(Options),
            do_find_numbers(Prefix, Quantity, Offset, AccountId, QID)
    end.

-spec do_find_numbers(ne_binary(), pos_integer(), non_neg_integer(), ne_binary(), ne_binary()) ->
                             {'ok', knm_number:knm_numbers()} |
                             {'error', any()}.
do_find_numbers(<<"+",_/binary>>=Prefix, Quantity, Offset, AccountId, QID)
  when is_integer(Quantity), Quantity > 0 ->
    Local = erlang:atom_to_binary(?MODULE, 'utf8'),
    ViewOptions = [{'startkey', [?NUMBER_STATE_AVAILABLE, Local, Prefix]}
                  ,{'endkey', [?NUMBER_STATE_AVAILABLE, Local, <<"\ufff0">>]}
                  ,{'limit', Quantity}
                  ,{skip, Offset}
                  ],
    case
        'undefined' /= (DB = knm_converters:to_db(Prefix))
        andalso kz_datamgr:get_results(DB, <<"numbers/status">>, ViewOptions)
    of
        'false' -> {'error', 'not_available'};
        {'ok', []} ->
            lager:debug("found no available local numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found available local numbers for account ~s", [AccountId]),
            Numbers = format_numbers(QID, JObjs),
            find_more(Prefix, Quantity, Offset, AccountId, length(Numbers), QID, Numbers);
        {'error', _R}=E ->
            lager:debug("failed to lookup available local numbers: ~p", [_R]),
            E
    end;
do_find_numbers(_, _, _, _, _) ->
    {'error', 'not_available'}.

-spec find_more(ne_binary(), pos_integer(), non_neg_integer(), ne_binary(), non_neg_integer(), ne_binary(), knm_number:knm_numbers()) ->
                       {'ok', knm_number:knm_numbers()}.
find_more(Prefix, Quantity, Offset, AccountId, NotEnough, QID, Numbers)
  when NotEnough < Quantity ->
    case do_find_numbers(Prefix, Quantity - NotEnough, Offset + NotEnough, AccountId, QID) of
        {'ok', MoreNumbers} -> {'ok', Numbers ++ MoreNumbers};
        _Error -> {'ok', Numbers}
    end;
find_more(_, _, _, _, _Enough, _, Numbers) ->
    {'ok', Numbers}.

format_numbers(QID, JObjs) ->
    [{QID, {knm_phone_number:number(PN), knm_phone_number:module_name(PN), knm_phone_number:state(PN), knm_phone_number:carrier_data(PN)}}
     || JObj <- JObjs,
        {ok, N} <- [knm_number:get(kz_doc:id(JObj))],
        PN <- [knm_number:phone_number(N)]
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
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
