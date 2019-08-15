%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Send test API notifications
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_tests).

-export([voicemail_to_email/1, voicemail_to_email/2
        ,voicemail_full/1, voicemail_full/2
        ,fax_inbound_to_email/1, fax_inbound_to_email/2
        ,skel/1, skel/2
        ]).

-include("teletype.hrl").

-spec voicemail_to_email(kz_term:ne_binary()) -> 'ok' |
                                                 {'ok', kz_json:objects()} |
                                                 {'error', any()}.
voicemail_to_email(?NE_BINARY=AccountId) ->
    case find_vmboxes(AccountId) of
        [] -> lager:debug("no voicemail boxes in account ~s", [AccountId]);
        VMBoxes -> find_vmbox_messages(AccountId, VMBoxes)
    end.

-spec find_vmboxes(kz_term:ne_binary()) -> kz_json:objects().
find_vmboxes(?NE_BINARY=AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>, ['include_docs']) of
        {'ok', VMBoxes} -> VMBoxes;
        {'error', _E} ->
            lager:debug("failed to query ~s for vm boxes: ~p", [AccountId, _E]),
            []
    end.

-spec find_vmbox_messages(kz_term:ne_binary(), kz_json:objects()) ->
                                 'ok' |
                                 {'ok', kz_json:objects()} |
                                 {'error', any()}.
find_vmbox_messages(_AccountId, []) ->
    lager:debug("no vmboxes had messages in ~p", [_AccountId]);
find_vmbox_messages(AccountId, [Box|Boxes]) ->
    BoxJObj = kz_json:get_value(<<"doc">>, Box),
    case kvm_messages:get(AccountId, BoxJObj) of
        [] -> find_vmbox_messages(AccountId, Boxes);
        Ms -> voicemail_to_email(AccountId, BoxJObj, Ms)
    end.

-spec voicemail_to_email(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                'ok' |
                                {'ok', kz_json:objects()} |
                                {'error', any()}.
voicemail_to_email(AccountId, ?NE_BINARY=VoicemailBoxId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', VMBox} = kz_datamgr:open_cache_doc(AccountDb, VoicemailBoxId),
    find_vmbox_messages(AccountId, [VMBox]).

-spec voicemail_to_email(kz_term:ne_binary(), kz_json:object(), kz_json:objects()) ->
                                {'ok', kz_json:objects()} |
                                {'error', any()}.
voicemail_to_email(AccountId, VMBox,  [Message|_]) ->
    MediaId = kz_json:get_value(<<"media_id">>, Message),
    Length = kz_json:get_value(<<"length">>, Message),
    CallId = kz_json:get_value(<<"call_id">>, Message),
    Prop = [{<<"From-User">>, <<"TestFromUser">>}
           ,{<<"From-Realm">>, <<"TestFromRealm">>}
           ,{<<"To-User">>, <<"TestToUser">>}
           ,{<<"To-Realm">>, <<"TestToRealm">>}
           ,{<<"Account-DB">>, kz_util:format_account_db(AccountId)}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Voicemail-Box">>, kz_doc:id(VMBox)}
           ,{<<"Voicemail-ID">>, MediaId}
           ,{<<"Caller-ID-Number">>, <<"CallerIdNumber">>}
           ,{<<"Caller-ID-Name">>, <<"CallerIdName">>}
           ,{<<"Voicemail-Timestamp">>, kz_time:now_s()}
           ,{<<"Voicemail-Length">>, Length}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kz_amqp_worker:call_collect(Prop
                               ,fun kapi_notifications:publish_voicemail_new/1
                               ,5 * ?MILLISECONDS_IN_SECOND
                               ).

-spec skel(kz_term:ne_binary()) -> ok.
skel(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, <<"users/crossbar_listing">>, ['include_docs']) of
        {'ok', Users} -> find_user_for_skel(AccountId, Users);
        {'error', _E} -> lager:debug("failed to find users for ~s: ~p", [AccountId, _E])
    end.

find_user_for_skel(_AccountId, []) ->
    lager:debug("no users found for ~s", [_AccountId]);
find_user_for_skel(AccountId, [User|Users]) ->
    Keys = [[<<"doc">>, <<"email">>]
           ,[<<"doc">>, <<"username">>]
           ],
    case kz_json:get_first_defined(Keys, User) of
        'undefined' -> find_user_for_skel(AccountId, Users);
        PotentialEmail -> find_user_for_skel(AccountId, User, Users, PotentialEmail)
    end.

find_user_for_skel(AccountId, User, Users, PotentialEmail) ->
    case binary:split(PotentialEmail, <<"@">>) of
        [_U, _D] -> skel(AccountId, kz_doc:id(User));
        _ -> find_user_for_skel(AccountId, Users)
    end.

-spec skel(kz_term:ne_binary(), kz_term:ne_binary()) -> ok.
skel(AccountId, ?NE_BINARY=UserId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"User-ID">>, UserId}
          ,{<<"Preview">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_skel/1).

-spec voicemail_full(kz_term:ne_binary()) -> ok.
voicemail_full(AccountId) ->
    case find_vmboxes(AccountId) of
        [] -> lager:debug("there are no voicemail boxes in ~s", [AccountId]);
        [Box|_] -> voicemail_full(AccountId, kz_json:get_value(<<"doc">>, Box))
    end.

-spec voicemail_full(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
                            kz_amqp_worker:request_return().
voicemail_full(AccountId, ?NE_BINARY=BoxId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', Box} = kz_datamgr:open_cache_doc(AccountDb, BoxId),
    voicemail_full(AccountId, Box);
voicemail_full(AccountId, Box) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Props = [{<<"Account-DB">>, AccountDb}
            ,{<<"Account-ID">>, AccountId}
            ,{<<"Voicemail-Box">>, kz_doc:id(Box)}
            ,{<<"Max-Message-Count">>, 1}
            ,{<<"Message-Count">>, 2}
            ,{<<"Preview">>, 'true'}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Publisher = fun kapi_notifications:publish_voicemail_full/1,
    kz_amqp_worker:call_collect(Props, Publisher, 5 * ?MILLISECONDS_IN_SECOND).

-spec fax_inbound_to_email(kz_term:ne_binary()) -> ok.
fax_inbound_to_email(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, <<"faxes/crossbar_listing">>, ['include_docs']) of
        {'ok', Faxes} -> find_fax_with_attachment(AccountId, Faxes);
        {'error', _E} ->
            lager:debug("failed to find faxes: ~p", [_E])
    end.

find_fax_with_attachment(_AccountId, []) ->
    lager:debug("failed to find fax with attachment in ~s", [_AccountId]);
find_fax_with_attachment(AccountId, [Fax|Faxes]) ->
    case kz_doc:attachment_names(kz_json:get_value(<<"doc">>, Fax)) of
        [] -> find_fax_with_attachment(AccountId, Faxes);
        _As -> fax_inbound_to_email(AccountId, kz_json:get_value(<<"doc">>, Fax))
    end.

-spec fax_inbound_to_email(kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) ->
                                  kz_amqp_worker:request_return().
fax_inbound_to_email(AccountId, ?NE_BINARY=FaxId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', Fax} = kz_datamgr:open_cache_doc(AccountDb, FaxId),
    fax_inbound_to_email(AccountId, Fax);
fax_inbound_to_email(AccountId, Fax) ->
    Message = props:filter_undefined(
                [{<<"Fax-ID">>, kz_doc:id(Fax)}
                ,{<<"Owner-ID">>, kz_json:get_value(<<"owner_id">>, Fax)}
                ,{<<"FaxBox-ID">>, kz_json:get_value(<<"faxbox_id">>, Fax)}
                ,{<<"Account-ID">>, AccountId}
                 | notify_fields(Fax)
                ]),
    lager:debug("publishing fax inbound to email req for ~s/~s", [AccountId, kz_doc:id(Fax)]),
    Publisher = fun kapi_notifications:publish_fax_inbound/1,
    kz_amqp_worker:call_collect(Message, Publisher, 2 * ?MILLISECONDS_IN_SECOND).

-spec notify_fields(kz_json:object()) -> kz_term:proplist().
notify_fields(JObj) ->
    props:filter_empty(
      [{<<"From-User">>, <<"FromUser">>}
      ,{<<"From-Realm">>, <<"FromRealm">>}
      ,{<<"To-User">>, <<"ToUser">>}
      ,{<<"To-Realm">>, <<"ToRealm">>}
      ,{<<"Fax-Info">>, kz_json:get_value(<<"rx_results">>, JObj)}
      ,{<<"Caller-ID-Number">>, <<"CID-Number">>}
      ,{<<"Caller-ID-Name">>, <<"CID-Name">>}
      ,{<<"Callee-ID-Number">>, <<"Callee-Number">>}
      ,{<<"Callee-ID-Name">>, <<"Callee-Name">>}
      ,{<<"Call-ID">>, kz_json:get_value(<<"call_id">>, JObj)}
      ,{<<"Fax-Timestamp">>, kz_time:now_s()}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
