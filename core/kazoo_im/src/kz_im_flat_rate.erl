%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_im_flat_rate).

-include("kazoo_im.hrl").

-export([debit/1]).

-define(IM_LEDGER, <<"im">>).

-type ledger_result() :: {'ok', kz_ledger:ledger()} | {'error', any()}.
-type ledgers_result() :: {{'account' | 'reseller', kz_term:ne_binary()}, ledger_result()}.
-type ledgers_results() :: [ledgers_result()].

%%%------------------------------------------------------------------------------
%%% @doc create a ledger entry for the account and the Reseller
%%% @end
%%%------------------------------------------------------------------------------
-spec create_ledgers(kapps_im:im()) -> ledgers_results().
create_ledgers(IM) ->
    Ids = case {kapps_im:account_id(IM), kapps_im:reseller_id(IM)} of
              {AccountId, AccountId} ->
                  [{'account', AccountId}];
              {AccountId, ResellerId} ->
                  [{'account', AccountId}
                  ,{'reseller', ResellerId}
                  ]
          end,
    [{Pair, create_ledger(IM, Id)} || {_, Id} = Pair <- Ids].

-spec create_ledger(kapps_im:im(), kz_term:api_ne_binary()) -> ledger_result().
create_ledger(IM, AccountId) ->
    Rate = kz_services_im:flat_rate(AccountId, kapps_im:type(IM), kapps_im:direction(IM)),
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, AccountId}
          ,{fun kz_ledger:set_source_service/2, ?IM_LEDGER}
          ,{fun kz_ledger:set_source_id/2, kapps_im:message_id(IM)}
          ,{fun kz_ledger:set_description/2, description(IM)}
          ,{fun kz_ledger:set_usage_type/2, kz_term:to_binary(kapps_im:type(IM))}
          ,{fun kz_ledger:set_usage_quantity/2, 1}
          ,{fun kz_ledger:set_usage_unit/2, <<"message">>}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_metadata/2, metadata(IM, AccountId)}
          ,{fun kz_ledger:set_dollar_amount/2, Rate}
          ]
         ),
    kz_ledger:debit(kz_ledger:setters(Setters), AccountId).

-spec description(kapps_im:im()) -> kz_term:ne_binary().
description(IM) ->
    list_to_binary([kz_term:to_binary(kapps_im:direction(IM))
                   ," from "
                   ,kapps_im:from(IM)
                   ," to "
                   ,kapps_im:to(IM)
                   ]).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec debit(kapps_im:im()) -> 'ok'.
debit(IM) ->
    case lists:filter(fun filter_ledger_error/1, create_ledgers(IM)) of
        [] -> 'ok';
        Errors -> hd([lager:error("failed to create ~s/~s ledger => ~p", [T, Id, Error])
                      || {{T, Id}, {'error', Error}} <- Errors])
    end.

filter_ledger_error({_, {'error', _}}) -> 'true';
filter_ledger_error(_) -> 'false'.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec metadata(kapps_im:im(), kz_term:ne_binary()) -> kz_json:object().
metadata(IM, AccountId) ->
    add_metadata(kapps_im:account_id(IM), AccountId).

-spec add_metadata(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
add_metadata(AccountId, AccountId) ->
    kz_json:new();
add_metadata(AccountId, _AccountId) ->
    kz_json:from_list([{<<"account_id">>, AccountId}]).
