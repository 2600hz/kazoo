%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Message Wait Indicator utilities.
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kvm_mwi).

-export([notify_vmbox/2
        ,notify_owner/2
        ,notify_endpoint/2
        ]).

-include("kz_voicemail.hrl").

-define(VM_NUMBER_KEY, <<"dialog_subscribed_mwi_prefix">>).
-define(VM_NUMBER(A), kapps_account_config:get_global(A, ?VM_CONFIG_CAT, ?VM_NUMBER_KEY)).

-define(VM_NO_NEW_MESSAGES, <<"terminated">>).
-define(VM_HAS_NEW_MESSAGES, <<"confirmed">>).

-define(MWI_SEND_UNSOLICITED_UPDATES, <<"mwi_send_unsolicited_updates">>).

%%------------------------------------------------------------------------------
%% @doc Generate database name based on DocId
%% @end
%%------------------------------------------------------------------------------

-spec notify_vmbox(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
notify_vmbox(Account, BoxId) ->
    send_mwi_update(Account, BoxId).

-spec notify_owner(kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
notify_owner(Account, OwnerId) ->
    unsolicited_owner_mwi_update(Account, OwnerId).

-spec notify_endpoint(kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
notify_endpoint(Account, EndpointId) ->
    unsolicited_endpoint_mwi_update(Account, EndpointId).

-spec send_mwi_update(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_mwi_update(Account, BoxId) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(AccountId),
    Realm = kzd_accounts:fetch_realm(AccountId),
    {'ok', BoxJObj} = kz_datamgr:open_cache_doc(AccountDb, BoxId),
    OwnerId = kzd_voicemail_box:owner_id(BoxJObj),
    BoxNumber = kzd_voicemail_box:mailbox_number(BoxJObj),
    {New, Saved} = kvm_messages:count_non_deleted(AccountId, BoxId),
    lager:debug("sending mwi for vmbox ~s@~s (~b/~b)", [BoxNumber, Realm, New, Saved]),
    notify_owner(AccountDb, OwnerId),
    To = <<BoxNumber/binary, "@", Realm/binary>>,
    Command = [{<<"To">>, To}
              ,{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Extended-Presence-ID">>, extended_presence_id(AccountId, BoxJObj)}
              ,{<<"Call-ID">>, ?FAKE_CALLID(To)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_mwi_update/1).

-spec extended_presence_id(kz_term:ne_binary(), kz_object:object()) -> kz_term:api_binary().
extended_presence_id(AccountId, BoxJObj) ->
    BoxNumber = kzd_voicemail_box:mailbox_number(BoxJObj),
    case ?VM_NUMBER(AccountId) of
        'undefined' -> 'undefined';
        Prefix -> <<Prefix/binary, BoxNumber/binary>>
    end.

-spec unsolicited_owner_mwi_update(kz_term:api_binary(), kz_term:api_binary()) ->
          'ok' |
          kz_datamgr:data_error().
unsolicited_owner_mwi_update('undefined', _) ->
    lager:warning("unsolicited owner mwi update for undefined Account");
unsolicited_owner_mwi_update(_, 'undefined') ->
    lager:warning("unsolicited owner mwi update for undefined owner_id");
unsolicited_owner_mwi_update(Account, OwnerId) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(AccountId),
    MWIUpdate = is_unsolicited_mwi_enabled(AccountId),
    unsolicited_owner_mwi_update(AccountDb, OwnerId, MWIUpdate).

-spec unsolicited_owner_mwi_update(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> 'ok'.
unsolicited_owner_mwi_update(_AccountDb, _OwnerId, 'false') ->
    lager:debug("unsolicited mwi updated disabled : ~s", [_AccountDb]);
unsolicited_owner_mwi_update(AccountDb, OwnerId, 'true') ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            AccountId = kzs_util:format_account_id(AccountDb),
            lists:foreach(fun(JObj) -> maybe_send_unsolicited_mwi_update(JObj, AccountId, New, Saved) end
                         ,JObjs
                         ),
            'ok';
        {'error', _R} ->
            lager:warning("failed to find devices owned by ~s: ~p", [OwnerId, _R])
    end.

-spec maybe_send_unsolicited_mwi_update(kz_json:object(), kz_term:ne_binary(), integer(), integer()) -> 'ok'.
maybe_send_unsolicited_mwi_update(JObj, AccountId, New, Saved) ->
    J = kz_json:get_value(<<"doc">>, JObj),
    Username = kzd_devices:sip_username(J),
    Realm = kz_endpoint:get_sip_realm(J, AccountId),
    OwnerId = get_endpoint_owner(J),
    case <<"password">> =:= kzd_devices:sip_method(J)
        andalso 'undefined' =/= Username
        andalso 'undefined' =/= Realm
        andalso 'undefined' =/= OwnerId
        andalso kzd_devices:mwi_unsolicited_updates(J)
    of
        'true' -> send_unsolicited_mwi_update(New, Saved, Username, Realm);
        'false' -> 'ok'
    end.

-spec unsolicited_endpoint_mwi_update(kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
unsolicited_endpoint_mwi_update('undefined', _) ->
    lager:warning("unsolicited endpoint mwi update for undefined Account");
unsolicited_endpoint_mwi_update(_, 'undefined') ->
    lager:warning("unsolicited endpoint mwi update for undefined EndpointId");
unsolicited_endpoint_mwi_update(Account, EndpointId) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(AccountId),
    MWIUpdate = is_unsolicited_mwi_enabled(AccountId),
    unsolicited_endpoint_mwi_update(AccountDb, EndpointId, MWIUpdate).

-spec unsolicited_endpoint_mwi_update(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> 'ok'.
unsolicited_endpoint_mwi_update(_AccountDb, _EndpointId, 'false') ->
    lager:debug("unsolicited mwi updated disabled : ~s", [_AccountDb]);
unsolicited_endpoint_mwi_update(AccountDb, EndpointId, 'true') ->
    case kz_datamgr:open_cache_doc(AccountDb, EndpointId) of
        {'error', _Error} -> lager:error("opening endpoint document ~s from db ~s", [EndpointId, AccountDb]);
        {'ok', JObj} -> maybe_send_endpoint_mwi_update(AccountDb, JObj)
    end.

-spec maybe_send_endpoint_mwi_update(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_endpoint_mwi_update(AccountDb, JObj) ->
    maybe_send_endpoint_mwi_update(AccountDb, JObj, kzd_devices:mwi_unsolicited_updates(JObj)).

-spec maybe_send_endpoint_mwi_update(kz_term:ne_binary(), kz_json:object(), boolean()) -> 'ok'.
maybe_send_endpoint_mwi_update(_AccountDb, _JObj, 'false') ->
    lager:debug("unsolicited mwi updates disabled for ~s/~s", [_AccountDb, kz_doc:id(_JObj)]);
maybe_send_endpoint_mwi_update(AccountDb, JObj, 'true') ->
    AccountId = kzs_util:format_account_id(AccountDb),
    Username = kzd_devices:sip_username(JObj),
    Realm = kz_endpoint:get_sip_realm(JObj, AccountId),
    OwnerId = get_endpoint_owner(JObj),
    case <<"password">> =:= kzd_devices:sip_method(JObj)
        andalso 'undefined' =/= Username
        andalso 'undefined' =/= Realm
    of
        'false' -> {'error', 'not_appropriate'};
        'true' ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            send_unsolicited_mwi_update(New, Saved, Username, Realm)
    end.

%%------------------------------------------------------------------------------
%% @end
%%------------------------------------------------------------------------------
-type vm_count() :: non_neg_integer().
-spec send_unsolicited_mwi_update(vm_count(), vm_count(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_unsolicited_mwi_update(New, Saved, Username, Realm) ->
    send_unsolicited_mwi_update(New, Saved, Username, Realm, kz_json:new()).

-spec send_unsolicited_mwi_update(vm_count(), vm_count(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_unsolicited_mwi_update(New, Saved, Username, Realm, JObj) ->
    Command = [{<<"To">>, <<Username/binary, "@", Realm/binary>>}
              ,{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("sending unsolicited mwi update for ~s@~s (~p/~p)", [Username, Realm, New, Saved]),
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_unsolicited_mwi_update/1).


-spec is_unsolicited_mwi_enabled(kz_term:ne_binary()) -> boolean().
is_unsolicited_mwi_enabled(AccountId) ->
    kapps_config:get_is_true(?VM_CONFIG_CAT, ?MWI_SEND_UNSOLICITED_UPDATES, 'true')
        andalso kz_term:is_true(kapps_account_config:get(AccountId, ?VM_CONFIG_CAT, ?MWI_SEND_UNSOLICITED_UPDATES, 'true')).

-spec vm_count_by_owner(kz_term:ne_binary(), kz_term:api_binary()) -> {non_neg_integer(), non_neg_integer()}.
vm_count_by_owner(_AccountDb, 'undefined') -> {0, 0};
vm_count_by_owner(<<_/binary>> = AccountDb, <<_/binary>> = OwnerId) ->
    kvm_messages:count_by_owner(AccountDb, OwnerId).

%%------------------------------------------------------------------------------
%% @end
%%------------------------------------------------------------------------------
-spec get_endpoint_owner(kz_json:object()) -> kz_term:api_ne_binary().
get_endpoint_owner(JObj) ->
    maybe_get_endpoint_hotdesk_owner(JObj).

-spec maybe_get_endpoint_hotdesk_owner(kz_json:object()) -> kz_term:api_ne_binary().
maybe_get_endpoint_hotdesk_owner(JObj) ->
    case kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj) of
        [] -> maybe_get_endpoint_assigned_owner(JObj);
        [OwnerId] -> OwnerId;
        [_|_] -> 'undefined'
    end.

-spec maybe_get_endpoint_assigned_owner(kz_json:object()) -> kz_term:api_ne_binary().
maybe_get_endpoint_assigned_owner(JObj) ->
    kz_json:get_ne_binary_value(<<"owner_id">>, JObj).
