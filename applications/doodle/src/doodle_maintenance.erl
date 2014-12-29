%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(doodle_maintenance).

-include("doodle.hrl").

-export([send_outbound_sms/2, send_outbound_sms/3, send_outbound_sms/4, send_outbound_sms/5]).

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
check_sms_by_device_id(_AccountId, 'undefined') -> 'ok';
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
check_sms_by_owner_id(_AccountId, 'undefined') -> 'ok';
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
start_check_sms_by_account(AccountId, _JObj) ->
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

-define(DEFAULT_ROUTEID, whapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_route_id">>, <<"syneverse">>)).
-define(DEFAULT_FROM, whapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_from_number">>, <<"15552220001">>)).

send_outbound_sms(To, Msg) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg).

send_outbound_sms(To, Msg, Times) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg, Times).

send_outbound_sms(To, From, RouteId, Msg) ->
    Payload = [{<<"Message-ID">>, wh_util:rand_hex_binary(16)}
               ,{<<"System-ID">>, wh_util:node_name() }
               ,{<<"Route-ID">>, RouteId }
               ,{<<"From">>, From}
               ,{<<"To">>, wh_util:to_binary(To)}
               ,{<<"Body">>, Msg}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wh_amqp_worker:cast(Payload, fun wapi_sms:publish_outbound/1).

send_outbound_sms(To, From, RouteId, Msg, Times) ->
    [begin
      send_outbound_sms(To, From, RouteId, <<"MSG - ", (wh_util:to_binary(X))/binary, " => ", Msg/binary>>),
      timer:sleep(2000)
     end
       || X <- lists:seq(1, wh_util:to_integer(Times))].

