%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_util).

-include("doodle.hrl").

-define(SIP_ENDPOINT_ID_KEY(Realm, User)
       ,{?MODULE, 'sip_endpoint_id', Realm, User}
       ).
-define(SIP_ENDPOINT_KEY(Realm, User)
       ,{?MODULE, 'sip_endpoint', Realm, User}
       ).
-define(DEFAULT_INCEPTION, <<"offnet">>).
-define(MDN_VIEW, <<"mobile/listing_by_mdn">>).
-define(CONVERT_MDN, 'true').

-export([get_inbound_destination/1]).
-export([lookup_mdn/1]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_inbound_field(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_inbound_field(Inception) ->
    case Inception of
        <<"onnet">> -> [<<"Caller-ID-Number">>, <<"From">>];
        <<"offnet">> -> [<<"Callee-ID-Number">>, <<"To">>];
        _ -> get_inbound_field(?DEFAULT_INCEPTION)
    end.

-spec get_inbound_destination(kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_inbound_destination(JObj) ->
    Inception = kz_json:get_value(<<"Route-Type">>, JObj, ?DEFAULT_INCEPTION),
    Keys = get_inbound_field(Inception),
    Number = kz_json:get_first_defined(Keys, JObj),
    {knm_converters:normalize(Number), Inception}.

-spec lookup_mdn(kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
          {'error', any()}.
lookup_mdn(Number) ->
    Num = knm_converters:normalize(Number),
    case kz_cache:fetch_local(?CACHE_NAME, cache_key_mdn(Num)) of
        {'ok', {Id, OwnerId}} ->
            lager:debug("cached number ~s is associated with ~s/~s", [Num, OwnerId, Id]),
            {'ok', Id, OwnerId};
        {'error', 'not_found'} -> fetch_mdn(Num)
    end.

-spec fetch_mdn(kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
          {'error', any()}.
fetch_mdn(Num) ->
    case knm_numbers:lookup_account(Num) of
        {'ok', AccountId, _Props} ->
            fetch_mdn_result(AccountId, Num);
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec fetch_mdn_result(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
          {'error', 'not_found'}.
fetch_mdn_result(AccountId, Num) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    ViewOptions = [{'key', mdn_from_e164(Num)}],
    case kz_datamgr:get_single_result(AccountDb, ?MDN_VIEW, ViewOptions) of
        {'ok', JObj} ->
            Id = kz_doc:id(JObj),
            OwnerId = kz_json:get_value([<<"value">>, <<"owner_id">>], JObj),
            lager:debug("~s is associated with mobile device ~s in account ~s", [Num, Id, AccountId]),
            cache_mdn_result(AccountDb, Id, OwnerId);
        {'error', 'not_found'}=E -> E;
        {'error', _R}=E ->
            lager:warning("could not fetch mdn for ~p: ~p", [Num, _R]),
            E
    end.

-spec cache_mdn_result(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
          {'ok', kz_term:ne_binary(), kz_term:api_binary()}.
cache_mdn_result(AccountDb, Id, OwnerId) ->
    CacheProps = [{'origin', [{'db', AccountDb, Id}]}],
    kz_cache:store_local(?CACHE_NAME, cache_key_mdn(Id), {Id, OwnerId}, CacheProps),
    {'ok', Id, OwnerId}.

-spec cache_key_mdn(kz_term:ne_binary()) -> {'sms_mdn', kz_term:ne_binary()}.
cache_key_mdn(Number) ->
    {'sms_mdn', Number}.

%% TODO
%% MDN should be stored in e164 format
-spec mdn_from_e164(binary()) -> binary().
mdn_from_e164(<<"+1", Number/binary>>) -> Number;
mdn_from_e164(<<"1", Number/binary>>) -> Number;
mdn_from_e164(Number) -> Number.
