
%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(asr_flat_rate).

-include("kazoo_speech.hrl").

-export([authorize/1
        ,debit/1
        ]).

-define(ASR_LEDGER, <<"asr">>).

%%------------------------------------------------------------------------------
%% @doc
%% Authorize the ASR request has sufficient funds
%% @end
%%------------------------------------------------------------------------------
-spec authorize(asr_req()) -> asr_req().
authorize(#asr_req{account_id=AccountId, asr_provider=Provider, impact_reseller='false'}=Request) ->
    Services = kz_services:fetch(AccountId),
    Amount = kz_services_asr:flat_rate(AccountId, Provider),
    case maybe_consume_flat_rate(Services, Amount) of
        {'true', _} ->
            Request#asr_req{account_authorized='true', amount=Amount};
        {'false', _} -> asr_request:add_error(Request, {'error', 'insufficient_funds'})
    end;
authorize(#asr_req{account_id=AccountId, asr_provider=Provider, impact_reseller='true', reseller_id=ResellerId}=Request) ->
    Account = kz_services:fetch(AccountId),
    Reseller = kz_services:fetch(ResellerId),
    Amount = kz_services_asr:flat_rate(AccountId, Provider),
    case {maybe_consume_flat_rate(Account, Amount), maybe_consume_flat_rate(Reseller, Amount)} of
        {{'true', _}, {'true', _}} ->
            Request#asr_req{account_authorized='true', reseller_authorized='true', amount=Amount};
        {{'true', _}, {'false', _}} ->
            Request#asr_req{account_authorized='true', reseller_authorized='false', amount=Amount};
        {{'false', _}, {'true', _}} ->
            Request#asr_req{account_authorized='false', reseller_authorized='true', amount=Amount};
        {_,_} ->
            Request#asr_req{account_authorized='false', reseller_authorized='false', amount=Amount}
    end.

%%%------------------------------------------------------------------------------
%%% @doc create a ledger entry for the account and maybe the Reseller
%%% @end
%%%------------------------------------------------------------------------------
-spec create_ledger_entry(asr_req()) -> asr_req().
create_ledger_entry(#asr_req{account_id=AccountId, impact_reseller='false'}=Request) ->
    lager:debug("creating asr ledger entry."),
    create_ledger_entry(AccountId, Request);
create_ledger_entry(#asr_req{account_id=AccountId, reseller_id=ResellerId, impact_reseller='true'}=Request) ->
    lists:foldl(fun create_ledger_entry/2, Request, [AccountId, ResellerId]).

-spec create_ledger_entry(kz_term:ne_binary(), asr_req()) -> asr_req().
create_ledger_entry(AccountId, Request) ->
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, asr_request:account_id(Request)}
          ,{fun kz_ledger:set_source_service/2, ?ASR_LEDGER}
          ,{fun kz_ledger:set_source_id/2, asr_request:call_id(Request)}
          ,{fun kz_ledger:set_description/2, asr_request:description(Request)}
          ,{fun kz_ledger:set_usage_type/2, asr_request:asr_provider(Request)}
          ,{fun kz_ledger:set_usage_quantity/2, 1}
          ,{fun kz_ledger:set_usage_unit/2, <<"transcription">>}
          ,{fun kz_ledger:set_period_start/2, asr_request:timestamp(Request)}
          ,{fun kz_ledger:set_metadata/2, metadata(Request)}
          ,{fun kz_ledger:set_dollar_amount/2, asr_request:amount(Request)}
          ]
         ),
    case kz_ledger:debit(kz_ledger:setters(Setters), AccountId) of
        {'ok', _} -> Request;
        {'error', Msg} -> asr_request:add_error(Request, {'error', 'asr_provider_failure', kz_term:to_binary(Msg)})
    end.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec debit(asr_req()) -> asr_req().
debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    maybe_debit(Request);
debit(#asr_req{account_authorized='true', reseller_authorized='true', impact_reseller='true'}=Request) ->
    maybe_debit(Request);
debit(Request) ->
    asr_request:add_error(Request, {'error', 'asr_provider_failure', <<"unauthorized">>}).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec maybe_consume_flat_rate(kz_services:services(), kz_currency:dollars()) -> kz_services_standing:acceptable_return().
maybe_consume_flat_rate(Services, Amount) ->
    Options = #{amount => kz_currency:dollars_to_units(Amount)},
    kz_services_standing:acceptable(Services, Options).

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-spec maybe_debit(asr_req()) -> asr_req().
maybe_debit(#asr_req{amount=0}=Request) ->
    lager:debug("billable rate is 0 skipping ledger entry"),
    Request;
maybe_debit(#asr_req{account_authorized='true', impact_reseller='false'}=Request) ->
    lager:debug("impact reseller is false"),
    create_ledger_entry(Request);
maybe_debit(#asr_req{account_id=AccountId, reseller_id=ResellerId, account_authorized='true', reseller_authorized='true'}=Request) ->
    lager:debug("impact reseller is true updating accounts [~s, ~s]", [AccountId, ResellerId]),
    create_ledger_entry(Request);
maybe_debit(Request) -> Request.

%%%------------------------------------------------------------------------------
%%% @doc
%%% @end
%%%------------------------------------------------------------------------------
-spec metadata(asr_req()) -> kz_json:object().
metadata(Request) ->
    kz_json:from_list([{<<"account_id">>, asr_request:account_id(Request)}
                      ,{<<"call_id">>, asr_request:call_id(Request)}
                      ,{<<"media_id">>, asr_request:media_id(Request)}
                      ,{<<"provider">>, asr_request:asr_provider(Request)}
                      ,{<<"recording_seconds">>, asr_request:recording_seconds(Request)}
                      ]).
