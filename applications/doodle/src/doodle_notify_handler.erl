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

-export([replay_sms/2]).
-export([check_sms_by_device_id/2, check_sms_by_owner_id/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = wapi_notifications:register_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),    
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    DeviceId = wh_json:get_value(<<"Authorizing-ID">>, JObj),
    OwnerId = wh_json:get_value(<<"Owner-ID">>, JObj),
    spawn(?MODULE, 'check_sms_by_device_id', [AccountId, DeviceId]),
    spawn(?MODULE, 'check_sms_by_owner_id', [AccountId, OwnerId]).
    
-spec check_sms_by_device_id(ne_binary(), ne_binary()) -> any().
check_sms_by_device_id(AccountId, DeviceId) ->
    ViewOptions = [{'key', DeviceId}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_device">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no sms for device ~s in account ~s", [DeviceId, AccountId]);
        {'ok', JObjs} ->
            [replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj) ) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get sms by device for ~s/~s: ~p", [AccountId, DeviceId, _R])
    end.


-spec check_sms_by_owner_id(ne_binary(), ne_binary()) -> any().
check_sms_by_owner_id(AccountId, OwnerId) ->
    ViewOptions = [{'key', OwnerId}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_owner">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no sms for owner_id ~s in account ~s",[OwnerId, AccountId]);
        {'ok', JObjs} ->
            [replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj) ) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get sms by owner_id for owner_id ~s in account ~s: ~p", [AccountId, OwnerId, _R])
    end.

-spec replay_sms(ne_binary(), ne_binary()) -> any().
replay_sms(AccountId, DocId) ->
    lager:debug("trying to replay sms ~s for account ~s",[DocId, AccountId]),
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = wh_json:get_value(<<"pvt_call">>, Doc),
    replay_sms_flow(AccountId, DocId, Flow).

replay_sms_flow(AccountId, DocId,'undefined') -> 'ok';
replay_sms_flow(AccountId, DocId, JObj) ->
    lager:debug("replaying sms ~s for account ~s",[DocId, AccountId]),
    Call = whapps_call:from_json(JObj),
    whapps_call:set_account_id(AccountId, Call),
    whapps_call:put_callid(Call),
    lager:info("doodle received sms resume for ~s of account ~s, taking control",[DocId, AccountId]),
    doodle_route_win:maybe_restrict_call(JObj, Call).
