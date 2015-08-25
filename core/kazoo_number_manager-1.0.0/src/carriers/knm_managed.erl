%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(knm_managed).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([generate_numbers/3]).
-export([import_numbers/2]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../knm.hrl").

-define(WH_MANAGED, <<"numbers%2Fmanaged">>).
-define(MANAGED_VIEW_FILE, <<"views/managed.json">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          {'error', 'non_available'}.
find_numbers(<<"+", _/binary>>=Number, Quantity, Opts) ->
    AccountId = props:get_value(<<"Account-ID">>, Opts),
    find_numbers_in_account(Number, Quantity, AccountId);
find_numbers(Number, Quantity, Opts) ->
    find_numbers(<<"+",Number/binary>>, Quantity, Opts).

-spec find_numbers_in_account(ne_binary(), pos_integer(), api_binary()) ->
                                     {'error', _} |
                                     {'ok', wh_json:object()}.
find_numbers_in_account(Number, Quantity, AccountId) ->
    case do_find_numbers_in_account(Number, Quantity, AccountId) of
        {'error', 'non_available'}=A ->
            case wh_services:find_reseller_id(AccountId) of
                AccountId -> A;
                ResellerId -> find_numbers_in_account(Number, Quantity, ResellerId)
            end;
        R -> R
    end.

-spec do_find_numbers_in_account(ne_binary(), pos_integer(), api_binary()) ->
                                        {'error', _} |
                                        {'ok', wh_json:object()}.
do_find_numbers_in_account(Number, Quantity, AccountId) ->
    ViewOptions = [{'startkey', [AccountId, ?NUMBER_STATE_AVAILABLE, Number]}
                   ,{'endkey', [AccountId, ?NUMBER_STATE_AVAILABLE, <<Number/binary, "\ufff0">>]}
                   ,{'limit', Quantity}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_MANAGED, <<"numbers/status">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("found no available managed numbers for account ~p", [AccountId]),
            {'error', 'non_available'};
        {'ok', JObjs} ->
            lager:debug("found ~p available managed numbers for account ~p", [length(JObjs),AccountId]),
            {'ok', format_numbers_resp(JObjs)};
        {'error', _R}=E ->
            lager:debug("failed to lookup available managed numbers: ~p", [_R]),
            E
    end.

-spec format_numbers_resp(wh_json:objects()) ->
                                 wh_json:object().
format_numbers_resp(JObjs) ->
    Numbers = lists:foldl(fun format_numbers_resp_fold/2, [], JObjs),
    wh_json:from_list(Numbers).

-type format_acc() :: wh_json:json_proplist().

-spec format_numbers_resp_fold(wh_json:object(), format_acc()) -> format_acc().
format_numbers_resp_fold(JObj, Acc) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Id = wh_doc:id(Doc),
    Props = props:filter_undefined(
              [{<<"number">>, Id}
               ,{<<"rate">>, wh_json:get_value(<<"rate">>, Doc, <<"1">>)}
               ,{<<"activation_charge">>, wh_json:get_value(<<"activation_charge">>, Doc, <<"0">>)}
              ]),
    [{Id, wh_json:from_list(Props)} | Acc].

-spec is_number_billable(knm_number:knm_number()) -> boolean().
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            {'ok', knm_number:knm_number()}.
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_phone_number:number(PhoneNumber),
    AssignTo = knm_phone_number:assigned_to(PhoneNumber),
    State = knm_phone_number:state(PhoneNumber),
    lager:debug("acquiring number ~s in managed provider", [Num]),
    update_doc(Number, [{?PVT_STATE, State}
                        ,{<<"pvt_assigned_to">>, AssignTo}
                       ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number_return().
disconnect_number(Number) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    lager:debug("disconnect number ~s in managed provider", [Num]),
    update_doc(Number, [{?PVT_STATE, ?NUMBER_STATE_RELEASED}
                        ,{<<"pvt_assigned_to">>,<<>>}
                       ]).

-spec generate_numbers(ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(_AccountId, _Number, 0) -> 'ok';
generate_numbers(AccountId, Number, Quantity)
  when Quantity > 0
       andalso is_integer(Number)
       andalso is_integer(Quantity) ->
    save_doc(AccountId, Number),
    generate_numbers(AccountId, Number+1, Quantity-1).

-spec import_numbers(ne_binary(), ne_binaries()) ->
                            wh_json:object().
-spec import_numbers(ne_binary(), ne_binaries(), wh_json:object()) ->
                            wh_json:object().

import_numbers(_AccountId, Numbers) ->
    import_numbers(_AccountId, Numbers, wh_json:new()).

import_numbers(_AccountId, [], JObj) -> JObj;
import_numbers(AccountId, [Number | Numbers], JObj) ->
    NewJObj = case save_doc(AccountId, Number) of
                  {'ok', _Doc} ->
                      wh_json:set_value([<<"success">>, Number], wh_json:new(), JObj);
                  {'error', Reason} ->
                      Error = wh_json:from_list([{<<"reason">>, Reason}
                                                 ,{<<"message">>, <<"error adding number to couchdb">>}
                                                ]),
                      wh_json:set_value([<<"errors">>, Number], Error, JObj)
              end,
    import_numbers(AccountId, Numbers, NewJObj).

-spec save_doc(ne_binary(), pos_integer() | ne_binary()) ->
                      {'ok', wh_json:object()} |
                      {'error', _}.
save_doc(AccountId, Number) ->
    JObj = wh_json:from_list([{<<"_id">>,<<"+",(wh_util:to_binary(Number))/binary>>}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
                              ,{<<"pvt_type">>, <<"number">>}
                             ]),
    save_doc(JObj).

save_doc(JObj) ->
    case couch_mgr:save_doc(?WH_MANAGED, JObj) of
        {'error', 'not_found'} ->
            create_managed_db(),
            save_doc(JObj);
        R -> R
    end.

-spec update_doc(knm_number:knm_number(), wh_proplist()) ->
                        {'ok', knm_number:knm_number()}.
update_doc(Number, UpdateProps) ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case couch_mgr:update_doc(?WH_MANAGED, wh_doc:id(Doc), UpdateProps) of
        {'error', Error} ->
            knm_errors:unspecified(Error, Number);
        {'ok', UpdatedDoc} ->
            {'ok'
             ,knm_number:set_phone_number(
                Number
                ,knm_phone_number:from_json(UpdatedDoc)
               )
            }
    end.

-spec create_managed_db() -> 'ok'.
create_managed_db() ->
    _ = couch_mgr:db_create(?WH_MANAGED),
    _ = couch_mgr:revise_doc_from_file(?WH_MANAGED, 'kazoo_number_manager', ?MANAGED_VIEW_FILE),
    'ok'.

-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
