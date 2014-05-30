%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_notify_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    lager:info("DOODLE ~p",[JObj]),
    'true' = wapi_notifications:register_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            lager:debug("checking sms messages for ~s@~s",[Username, Realm]),
            process_pending_messages(Username, Realm, AccountDb, JObj);
        _Else -> 'ok'
    end.

-spec process_pending_messages(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
process_pending_messages(Username, Realm, AccountDb, JObj) ->
    'ok'.
%    case owner_ids_by_sip_username(AccountDb, Username) of
%        {'ok', [OwnerId]} ->
%            presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj);
%        _Else -> 'ok'
%    end.

-spec presence_mwi_resp(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj) ->
    {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
    send_mwi_update(New, Saved, Username, Realm, JObj).


-spec vm_count_by_owner(ne_binary(), api_binary()) -> {non_neg_integer(), non_neg_integer()}.
vm_count_by_owner(_, 'undefined') ->
    {0, 0};
vm_count_by_owner(AccountDb, OwnerId) ->
    ViewOptions = [{'reduce', 'true'}
                   ,{'group', 'true'}
                   ,{'group_level', 2}
                   ,{'startkey', [OwnerId]}
                   ,{'endkey', [OwnerId, "\ufff0"]}
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/vm_count_by_owner">>, ViewOptions) of
        {'ok', MessageCounts} ->
            Props = [{wh_json:get_value([<<"key">>, 2], MessageCount), wh_json:get_value(<<"value">>, MessageCount)}
                     || MessageCount <- MessageCounts
                    ],
            {props:get_value(<<"new">>, Props, 0), props:get_value(<<"saved">>, Props, 0)};
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

-type vm_count() :: ne_binary() | non_neg_integer().
-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm) ->
    send_mwi_update(New, Saved, Username, Realm, wh_json:new()).

-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm, JObj) ->
    DefaultAccount = <<"sip:", Username/binary, "@", Realm/binary>>,
    Command = [{<<"Messages-New">>, New}
               ,{<<"Messages-Saved">>, Saved}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               ,{<<"Switch-Nodename">>, wh_json:get_value(<<"Switch-Nodename">>, JObj)}
               ,{<<"Subscription-Call-ID">>, wh_json:get_value(<<"Subscription-Call-ID">>, JObj)}
               ,{<<"Notify-User">>, Username}
               ,{<<"Notify-Realm">>, Realm}
               ,{<<"Message-Account">>, wh_json:get_value(<<"Message-Account">>, JObj, DefaultAccount)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("updating MWI for ~s (~b/~b)", [DefaultAccount, New, Saved]),
    whapps_util:amqp_pool_send(Command, fun wapi_notifications:publish_mwi_update/1).


