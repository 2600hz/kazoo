%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Allow resellers directly below the master account to find
%%% manually-added `available' numbers.
%%%
%%%
%%% @author Karl Anderson
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_local).
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
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
          knm_search:mod_response().
find_numbers(Prefix, Quantity, Options) ->
    case knm_carriers:account_id(Options) of
        'undefined' -> {'error', 'not_available'};
        AccountId ->
            Offset = knm_carriers:offset(Options),
            QID = knm_search:query_id(Options),
            do_find_numbers(Prefix, Quantity, Offset, AccountId, QID)
    end.

-spec do_find_numbers(kz_term:ne_binary(), pos_integer(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          knm_search:mod_response().
do_find_numbers(<<"+",_/binary>>=Prefix, Quantity, Offset, AccountId, QID)
  when is_integer(Quantity), Quantity > 0 ->
    Local = erlang:atom_to_binary(?MODULE, 'utf8'),
    ViewOptions = [{'startkey', [?NUMBER_STATE_AVAILABLE, Local, Prefix]}
                  ,{'endkey', [?NUMBER_STATE_AVAILABLE, Local, <<"\ufff0">>]}
                  ,{'limit', Quantity}
                  ,{'skip', Offset}
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
        {'error', _R} ->
            lager:debug("failed to lookup available local numbers: ~p", [_R]),
            {'error', 'datastore_fault'}
    end;
do_find_numbers(_, _, _, _, _) ->
    {'error', 'not_available'}.

-spec find_more(kz_term:ne_binary(), pos_integer(), non_neg_integer(), kz_term:ne_binary(), non_neg_integer(), kz_term:ne_binary(), knm_search:results()) ->
          {'ok', knm_search:results()}.
find_more(Prefix, Quantity, Offset, AccountId, NotEnough, QID, Numbers)
  when NotEnough < Quantity ->
    case do_find_numbers(Prefix, Quantity - NotEnough, Offset + NotEnough, AccountId, QID) of
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

-spec disconnect_number(knm_phone_number:record()) ->
          knm_phone_number:record().
disconnect_number(PN) -> PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
