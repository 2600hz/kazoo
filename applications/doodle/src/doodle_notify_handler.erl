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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_notifications:register_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),    
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    DeviceId = wh_json:get_value(<<"Authorizing-ID">>, JObj),
    check_sms_by_device(AccountId, DeviceId).
    
check_sms_by_device(AccountId, DeviceId) ->
    ViewOptions = [{'key', [DeviceId]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_device">>, ViewOptions) of
        {'ok', []} -> check_sms_by_owner(AccountId, cf_attributes:owner_id(DeviceId));
        {'ok', JObjs} ->
            [ replay_sms(AccountId, wh_json:get_value(<<"_id">>, JObj) )
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get sms by device for ~s/~s: ~p", [AccountId, DeviceId, _R]),
            check_sms_by_owner(AccountId, cf_attributes:owner_id(DeviceId))
    end.

check_sms_by_owner(AccountId, OwnerId) ->
    ViewOptions = [{'key', [OwnerId]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_ownerid">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("no sms for owner_id ~s/~s",[AccountId, OwnerId]);
        {'ok', JObjs} ->
            [ replay_sms(AccountId, wh_json:get_value(<<"_id">>, JObj) )
             || JObj <- JObjs
            ];
        {'error', _R} ->
            lager:debug("unable to get sms by owner_id for ~s/~s: ~p", [AccountId, OwnerId, _R])
    end.

-spec replay_sms(ne_binary(), ne_binary()) -> any().
replay_sms(AccountId, DocId) ->
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = wh_json:get_value(<<"pvt_call">>, Doc),
    replay_sms_flow(Flow).

replay_sms_flow('undefined') -> 'ok';
replay_sms_flow(JObj) ->
    Call = whapps_call:from_json(JObj),
    whapps_call:put_callid(Call),
    lager:info("received call resume, taking control"),
    doodle_route_win:maybe_restrict_call(JObj, Call).
