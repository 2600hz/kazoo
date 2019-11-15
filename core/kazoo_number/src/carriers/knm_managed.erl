%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_managed).
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
-export([import_numbers/2]).

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
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(<<"+", _/binary>>=Prefix, Quantity, Options) ->
    AccountId = knm_search:account_id(Options),
    find_numbers_in_account(Prefix, Quantity, AccountId, Options);
find_numbers(Prefix, Quantity, Options) ->
    find_numbers(<<"+",Prefix/binary>>, Quantity, Options).

-spec find_numbers_in_account(kz_term:ne_binary(), pos_integer(), kz_term:api_ne_binary(), knm_search:options()) ->
                                     {'ok', knm_number:knm_numbers()} |
                                     {'error', any()}.
find_numbers_in_account(Prefix, Quantity, AccountId, Options) ->
    case do_find_numbers_in_account(Prefix, Quantity, AccountId, Options) of
        {'error', 'not_available'}=Error ->
            ResellerId = knm_search:reseller_id(Options),
            case AccountId =:= 'undefined'
                orelse AccountId =:= ResellerId
            of
                'true' -> Error;
                'false' ->
                    find_numbers_in_account(Prefix, Quantity, ResellerId, Options)
            end;
        Result -> Result
    end.

-spec do_find_numbers_in_account(kz_term:ne_binary(), pos_integer(), kz_term:api_ne_binary(), knm_search:options()) ->
                                        {'ok', list()} |
                                        {'error', any()}.
do_find_numbers_in_account(Prefix, Quantity, AccountId, Options) ->
    ViewOptions = [{'startkey', [AccountId, ?NUMBER_STATE_AVAILABLE, Prefix]}
                  ,{'endkey', [AccountId, ?NUMBER_STATE_AVAILABLE, <<Prefix/binary,"\ufff0">>]}
                  ,{'limit', Quantity}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_MANAGED_DB, <<"numbers_managed/status">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("found no available managed numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found ~B available managed numbers for account ~s", [length(JObjs), AccountId]),
            format_numbers_resp(JObjs, Options);
        {'error', _R}=E ->
            lager:debug("failed to lookup available managed numbers: ~p", [_R]),
            E
    end.

-spec format_numbers_resp(kz_json:objects(), knm_search:options()) -> {'ok', list()}.
format_numbers_resp(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    Numbers = [format_number_resp(QID, JObj) || JObj <- JObjs],
    {'ok', Numbers}.

format_number_resp(QID, JObj) ->
    Num = kz_doc:id(kz_json:get_value(<<"doc">>, JObj)),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, kz_json:new()}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assigned_to(PhoneNumber),
    State = knm_phone_number:state(PhoneNumber),
    lager:debug("acquiring number ~s", [knm_phone_number:number(PhoneNumber)]),
    update_doc(Number, [{kzd_phone_numbers:pvt_state_path(), State}
                       ,{kzd_phone_numbers:pvt_assigned_to_path(), AssignTo}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    lager:debug("disconnecting number ~s"
               ,[knm_phone_number:number(knm_number:phone_number(Number))]
               ),
    update_doc(Number, [{kzd_phone_numbers:pvt_state_path(), ?NUMBER_STATE_AVAILABLE}
                       ,{kzd_phone_numbers:pvt_assigned_to_path(), 'null'}
                       ]).

-spec generate_numbers(kz_term:ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(_AccountId, _Number, 0) -> 'ok';
generate_numbers(?MATCH_ACCOUNT_RAW(AccountId), Number, Quantity)
  when Quantity > 0
       andalso is_integer(Number)
       andalso is_integer(Quantity) ->
    {'ok', _JObj} = save_doc(AccountId, <<"+",(kz_term:to_binary(Number))/binary>>),
    generate_numbers(AccountId, Number+1, Quantity-1).

-spec import_numbers(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
import_numbers(AccountId, Numbers) ->
    import_numbers(AccountId, Numbers, kz_json:new()).

-spec import_numbers(kz_term:ne_binary(), kz_term:ne_binaries(), kz_json:object()) -> kz_json:object().
import_numbers(_AccountId, [], JObj) -> JObj;
import_numbers(AccountId, [Number | Numbers], JObj) ->
    NewJObj =
        case save_doc(AccountId, Number) of
            {'ok', _Doc} ->
                kz_json:set_value([<<"success">>, Number], kz_json:new(), JObj);
            {'error', Reason} ->
                Error = kz_json:from_list([{<<"reason">>, Reason}
                                          ,{<<"message">>, <<"error adding number to DB">>}
                                          ]),
                kz_json:set_value([<<"errors">>, Number], Error, JObj)
        end,
    import_numbers(AccountId, Numbers, NewJObj).

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
    kz_datamgr:save_doc(?KZ_MANAGED_DB, kz_doc:update_pvt_parameters(kz_doc:setters(Setters), 'undefined', PvtOptions)).

-spec update_doc(knm_number:knm_number(), kz_term:proplist()) ->
                        knm_number:knm_number().
update_doc(Number, UpdateProps) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_phone_number:number(PhoneNumber),

    Updates = [{kzd_phone_numbers:pvt_module_name_path(), kz_term:to_binary(?MODULE)}
               | UpdateProps
              ],
    UpdateOptions = [{'update', Updates}],

    case kz_datamgr:update_doc(?KZ_MANAGED_DB, Num, UpdateOptions) of
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
