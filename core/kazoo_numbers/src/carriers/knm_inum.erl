%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Carrier for inums
%%% @author Karl Anderson
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_inum).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-export([generate_numbers/3]).

-include("knm.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 15
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
-spec check_numbers(kz_term:ne_binaries()) -> {ok, kz_json:object()} |
                                              {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%------------------------------------------------------------------------------
%% @doc Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(<<"+", _/binary>>=Prefix, Quantity, Options) ->
    AccountId = knm_carriers:account_id(Options),
    find_numbers_in_account(Prefix, Quantity, AccountId, Options);
find_numbers(Prefix, Quantity, Options) ->
    find_numbers(<<"+",Prefix/binary>>, Quantity, Options).

-spec find_numbers_in_account(kz_term:ne_binary(), pos_integer(), kz_term:api_ne_binary(), knm_carriers:options()) ->
                                     {'ok', knm_number:knm_numbers()} |
                                     {'error', any()}.
find_numbers_in_account(_Prefix, _Quantity, 'undefined', _Options) -> {'ok', []};
find_numbers_in_account(Prefix, Quantity, AccountId, Options) ->
    Offset = knm_carriers:offset(Options),
    QID = knm_search:query_id(Options),
    case do_find_numbers_in_account(Prefix, Quantity, Offset, AccountId, QID) of
        {'error', 'not_available'} ->
            ResellerId = knm_carriers:reseller_id(Options),
            case AccountId =:= ResellerId
            of
                'true' -> {'ok', []};
                'false' ->
                    NewOptions = [{'offset', 0} | Options],
                    find_numbers_in_account(Prefix, Quantity, ResellerId, NewOptions)
            end;
        Result -> Result
    end.

-spec do_find_numbers_in_account(kz_term:ne_binary(), pos_integer(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                        {'ok', knm_number:knm_numbers()} |
                                        {'error', any()}.
do_find_numbers_in_account(Prefix, Quantity, Offset, AccountId, QID) ->
    ViewOptions = [{'startkey', [AccountId, ?NUMBER_STATE_AVAILABLE, Prefix]}
                  ,{'endkey', [AccountId, ?NUMBER_STATE_AVAILABLE, <<Prefix/binary,"\ufff0">>]}
                  ,{'limit', Quantity}
                  ,{'skip', Offset}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_INUM_DB, <<"numbers_inum/status">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("found no available inum numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found available inum numbers for account ~s", [AccountId]),
            {'ok', format_numbers_resp(QID, JObjs)};
        {'error', _R}=E ->
            lager:debug("failed to lookup available local numbers: ~p", [_R]),
            E
    end.

format_numbers_resp(QID, JObjs) ->
    [{QID, {Num, ?MODULE, ?NUMBER_STATE_AVAILABLE, kz_json:new()}}
     || JObj <- JObjs,
        Num <- [kz_doc:id(kz_json:get_value(<<"doc">>, JObj))]
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    lager:debug("acquiring number ~s", [knm_phone_number:number(PhoneNumber)]),
    update_doc(Number, [{kzd_phone_numbers:pvt_state_path(), knm_phone_number:state(PhoneNumber)}
                       ,{kzd_phone_numbers:pvt_assigned_to_path(), knm_phone_number:assigned_to(PhoneNumber)}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    lager:debug("disconnect number ~s in managed provider"
               ,[knm_phone_number:number(knm_number:phone_number(Number))]
               ),
    update_doc(Number, [{kzd_phone_numbers:pvt_state_path(), ?NUMBER_STATE_RELEASED}
                       ,{kzd_phone_numbers:pvt_assigned_to_path(), 'null'}
                       ,{kzd_phone_numbers:pvt_reserve_history_path(), []}
                       ]).

-spec generate_numbers(kz_term:ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(AccountId, <<"8835100",_/binary>> = Number, Quantity)
  when byte_size(Number) =:= 15 ->
    generate_numbers(AccountId, kz_term:to_integer(Number), kz_term:to_integer(Quantity));
generate_numbers(_AccountId, _Number, 0) -> 'ok';
generate_numbers(?MATCH_ACCOUNT_RAW(AccountId), Number, Quantity)
  when is_integer(Number),
       is_integer(Quantity),
       Quantity > 0 ->
    _R = save_doc(AccountId, <<"+",(kz_term:to_binary(Number))/binary>>),
    lager:info("number ~p/~p/~p", [Number, Quantity, _R]),
    generate_numbers(AccountId, Number+1, Quantity-1).


-spec save_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} |
                                                            {'error', any()}.
save_doc(AccountId, Number) ->
    Setters = [{fun kzd_phone_numbers:set_pvt_module_name/2, kz_term:to_binary(?MODULE)}
              ,{fun kzd_phone_numbers:set_pvt_state/2, ?NUMBER_STATE_AVAILABLE}
              ],
    PvtOptions = [{'account_id', AccountId}
                 ,{'id', knm_converters:normalize(Number)}
                 ,{'type', kzd_phone_numbers:type()}
                 ],
    kz_datamgr:save_doc(?KZ_INUM_DB, kz_doc:update_pvt_parameters(kz_doc:setters(Setters), 'undefined', PvtOptions)).

-spec update_doc(knm_number:knm_number(), kz_term:proplist()) ->
                        knm_number:knm_number().
update_doc(Number, UpdateProps) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_phone_number:number(PhoneNumber),

    Updates = [{kzd_phone_numbers:pvt_module_name_path(), kz_term:to_binary(?MODULE)}
               | UpdateProps
              ],
    UpdateOptions = [{'update', Updates}],

    case kz_datamgr:update_doc(?KZ_INUM_DB, Num, UpdateOptions) of
        {'ok', _UpdatedDoc} -> Number;
        {'error', Reason} ->
            knm_errors:database_error(Reason, PhoneNumber)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
