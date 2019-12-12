%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Find reserved numbers in an account's reseller account.
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_reserved_reseller).
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 10
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the local system for a quantity of reserved numbers
%% assigned to an account's reseller account.
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
          knm_search:mod_response().
find_numbers(Prefix, Quantity, Options) ->
    case knm_carriers:account_id(Options) of
        'undefined' -> {'error', 'not_available'};
        _AccountId ->
            Offset = knm_carriers:offset(Options),
            QID = knm_search:query_id(Options),
            do_find_numbers(Prefix, Quantity, Offset, knm_carriers:reseller_id(Options), QID)
    end.

-spec do_find_numbers(kz_term:ne_binary(), pos_integer(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          knm_search:mod_response().
do_find_numbers(<<"+",_/binary>>=Prefix, Quantity, Offset, AccountId, QID)
  when is_integer(Quantity), Quantity > 0 ->
    AccountDb = kzs_util:format_account_db(AccountId),
    ViewOptions = [{'startkey', Prefix}
                  ,{'endkey', <<Prefix/binary, "\ufff0">>}
                  ,{'limit', Quantity}
                  ,{'skip', Offset}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"numbers/list_reserved">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no reserved numbers found"),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found reserved numbers in ~s", [AccountDb]),
            Numbers = format_numbers(QID, JObjs),
            find_more(Prefix, Quantity, Offset, AccountId, length(Numbers), QID, Numbers);
        {'error', _R} ->
            lager:debug("failed to lookup reserved numbers in ~s: ~p", [AccountDb, _R]),
            {'error', 'datastore_fault'}
    end;
do_find_numbers(_, _, _, _, _) ->
    {'error', 'not_available'}.

-spec find_more(kz_term:ne_binary(), pos_integer(), non_neg_integer(), kz_term:ne_binary(), non_neg_integer(), kz_term:ne_binary(), knm_search:results()) ->
          {'ok', knm_search:results()}.
find_more(Prefix, Quantity, Offset, AccountId, NotEnough, QID, Numbers)
  when NotEnough < Quantity ->
    case do_find_numbers(Prefix, Quantity - NotEnough, Offset + NotEnough, QID, AccountId) of
        {'ok', MoreNumbers} -> {'ok', Numbers ++ MoreNumbers};
        _Error -> {'ok', Numbers}
    end;
find_more(_, _, _, _, _Enough, _, Numbers) ->
    {'ok', Numbers}.

-spec format_numbers(kz_term:ne_binary(), kz_json:objects()) -> knm_search:results().
format_numbers(QID, JObjs) ->
    [{QID, {knm_phone_number:number(PN), knm_phone_number:module_name(PN), knm_phone_number:state(PN), knm_phone_number:carrier_data(PN)}}
     || PN <- knm_pipe:succeeded(knm_ops:get([kz_doc:id(JObj) || JObj <- JObjs]))
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_PN) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PN) -> PN.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------

-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(_PN) -> _PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.
