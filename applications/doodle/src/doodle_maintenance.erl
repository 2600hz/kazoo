%% @author root
%% @doc @todo Add description to doodle_maintenance.


-module(doodle_maintenance).

-include("doodle.hrl").

-export([flush/0]).
-export([check_sms_by_device_id/2, check_sms_by_owner_id/2]).
-export([start_check_sms_by_device_id/2, start_check_sms_by_owner_id/2]).
-export([check_pending_sms_for_outbound_delivery/1, start_check_sms_by_account/2]).


-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?DOODLE_CACHE).

-spec start_check_sms_by_device_id(ne_binary(), ne_binary()) -> any().
start_check_sms_by_device_id(AccountId, DeviceId) ->
    spawn(?MODULE, 'check_sms_by_device_id', [AccountId, DeviceId]).

-spec start_check_sms_by_owner_id(ne_binary(), ne_binary()) -> any().
start_check_sms_by_owner_id(AccountId, OwnerId) ->
    spawn(?MODULE, 'check_sms_by_owner_id', [AccountId, OwnerId]).

-spec check_sms_by_device_id(ne_binary(), ne_binary()) -> any().
check_sms_by_device_id(AccountId, 'undefined') -> 'ok';
check_sms_by_device_id(AccountId, DeviceId) ->
    ViewOptions = [{'key', DeviceId}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_device">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            [doodle_util:replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj) ) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get sms by device for ~s/~s: ~p", [AccountId, DeviceId, _R])
    end.

-spec check_sms_by_owner_id(ne_binary(), ne_binary()) -> any().
check_sms_by_owner_id(AccountId, 'undefined') -> 'ok';
check_sms_by_owner_id(AccountId, OwnerId) ->
    ViewOptions = [{'key', OwnerId}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_owner">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            [doodle_util:replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj) ) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get sms by owner_id for owner_id ~s in account ~s: ~p", [AccountId, OwnerId, _R])
    end.

-spec start_check_sms_by_account(ne_binary(), wh_json:object()) -> any().
start_check_sms_by_account(AccountId, JObj) ->
    spawn(?MODULE, 'check_pending_sms_for_outbound_delivery', [AccountId]).

-spec check_pending_sms_for_outbound_delivery(ne_binary()) -> any().
check_pending_sms_for_outbound_delivery(AccountId) ->
    ViewOptions = [],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_offnet">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            lager:debug("starting sms offnet delivery for account ~s", [AccountId]),
            [doodle_util:replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj) ) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get sms list for offnet delivery in account ~s : ~p", [AccountId, _R])
    end.
