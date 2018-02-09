%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% mwi
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kvm_mwi).

-export([notify/2]).

-include("kz_voicemail.hrl").

-define(VM_NUMBER_KEY, <<"dialog_subscribed_mwi_prefix">>).
-define(VM_NUMBER(A), kapps_account_config:get_global(A, ?VM_CONFIG_CAT, ?VM_NUMBER_KEY)).

-define(VM_NO_NEW_MESSAGES, <<"terminated">>).
-define(VM_HAS_NEW_MESSAGES, <<"confirmed">>).
%%--------------------------------------------------------------------
%% @public
%% @doc Generate database name based on DocId
%% @end
%%--------------------------------------------------------------------

-spec notify(kz_term:ne_binary(), kz_term:ne_binary()) -> pid().
notify(BoxId, Account) ->
    kz_util:spawn(fun() -> send_mwi_update(BoxId, Account) end).

-spec send_mwi_update(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_mwi_update(BoxId, Account) ->
    timer:sleep(?MILLISECONDS_IN_SECOND),

    AccountId = kz_util:format_account_id(Account),
    AccountDb = kz_util:format_account_db(AccountId),
    Realm = kzd_accounts:fetch_realm(AccountId),
    {'ok', BoxJObj} = kz_datamgr:open_cache_doc(AccountDb, BoxId),
    OwnerId = kzd_voicemail_box:owner_id(BoxJObj),
    BoxNumber = kzd_voicemail_box:mailbox_number(BoxJObj),
    {New, Saved} = kvm_messages:count_non_deleted(AccountId, BoxId),
    lager:debug("updating MWI for vmbox ~s@~s (~b/~b)", [BoxNumber, Realm, New, Saved]),
    kz_endpoint:unsolicited_owner_mwi_update(AccountDb, OwnerId),
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

