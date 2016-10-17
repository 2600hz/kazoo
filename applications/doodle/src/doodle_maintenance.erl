%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
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
-export([start_check_sms_by_account/2]).
-export([check_pending_sms_for_outbound_delivery/1]).
-export([check_pending_sms_for_delivery/1]).

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).

-spec start_check_sms_by_device_id(ne_binary(), ne_binary()) -> pid().
start_check_sms_by_device_id(AccountId, DeviceId) ->
    kz_util:spawn(fun check_sms_by_device_id/2, [AccountId, DeviceId]).

-spec start_check_sms_by_owner_id(ne_binary(), ne_binary()) -> pid().
start_check_sms_by_owner_id(AccountId, OwnerId) ->
    kz_util:spawn(fun check_sms_by_owner_id/2, [AccountId, OwnerId]).

-spec check_sms_by_device_id(ne_binary(), ne_binary()) -> 'ok'.
check_sms_by_device_id(_AccountId, 'undefined') -> 'ok';
check_sms_by_device_id(AccountId, DeviceId) ->
    ViewOptions = [{'endkey', [DeviceId, kz_util:current_tstamp()]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_device">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms by device for ~s/~s: ~p", [AccountId, DeviceId, _R])
    end.

-spec check_sms_by_owner_id(ne_binary(), api_binary()) -> 'ok'.
check_sms_by_owner_id(_AccountId, 'undefined') -> 'ok';
check_sms_by_owner_id(AccountId, OwnerId) ->
    ViewOptions = [{'endkey', [OwnerId, kz_util:current_tstamp()]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_owner">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms by owner_id for owner_id ~s in account ~s: ~p", [AccountId, OwnerId, _R])
    end.

-spec start_check_sms_by_account(ne_binary(), kz_json:object()) -> pid().
start_check_sms_by_account(AccountId, JObj) ->
    case kz_doc:is_soft_deleted(JObj)
        orelse kz_util:is_false(kz_json:get_value(<<"pvt_enabled">>, JObj, 'true'))
    of
        'true' -> 'ok';
        'false' -> kz_util:spawn(fun check_pending_sms_for_delivery/1, [AccountId])
    end.

-spec check_pending_sms_for_outbound_delivery(ne_binary()) -> pid().
check_pending_sms_for_outbound_delivery(AccountId) ->
    kz_util:spawn(fun check_pending_sms_for_offnet_delivery/1, [AccountId]),
    kz_util:spawn(fun check_queued_sms/1, [AccountId]).

-spec check_pending_sms_for_delivery(ne_binary()) -> 'ok'.
check_pending_sms_for_delivery(AccountId) ->
    ViewOptions = [{'limit', 100}
                  ,{'endkey', kz_util:current_tstamp()}
                  ],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms list for delivery in account ~s : ~p", [AccountId, _R])
    end.

-spec check_queued_sms(ne_binary()) -> 'ok'.
check_queued_sms(AccountId) ->
    ViewOptions = [{'limit', 100}],
    case kazoo_modb:get_results(AccountId, <<"sms/queued">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_queue_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get queued sms list in account ~s : ~p", [AccountId, _R])
    end.

-spec replay_queue_sms(ne_binary(), kz_json:objects()) -> 'ok'.
replay_queue_sms(AccountId, JObjs) ->
    lager:debug("starting queued sms for account ~s", [AccountId]),
    _ = [spawn_handler(AccountId, JObj)
         || JObj <- JObjs
        ],
    'ok'.

-spec spawn_handler(ne_binary(), kz_json:object()) -> 'ok'.
spawn_handler(AccountId, JObj) ->
    DocId = kz_doc:id(JObj),
    ?MATCH_MODB_PREFIX(Year,Month,_) = DocId,
    AccountDb = kazoo_modb:get_modb(AccountId, Year, Month),
    _ = kz_util:spawn(fun doodle_api:handle_api_sms/2, [AccountDb, DocId]),
    timer:sleep(200).

-spec check_pending_sms_for_offnet_delivery(ne_binary()) -> 'ok'.
check_pending_sms_for_offnet_delivery(AccountId) ->
    ViewOptions = [{'limit', 100}
                  ,{'endkey', kz_util:current_tstamp()}
                  ],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_offnet">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms list for offnet delivery in account ~s : ~p", [AccountId, _R])
    end.

-spec replay_sms(ne_binary(), kz_json:objects()) -> 'ok'.
replay_sms(AccountId, JObjs) ->
    lager:debug("starting sms offnet delivery for account ~s", [AccountId]),
    F = fun (JObj) ->
                doodle_util:replay_sms(AccountId, kz_doc:id(JObj)),
                timer:sleep(200)
        end,
    lists:foreach(F, JObjs).

-define(DEFAULT_ROUTEID,
        kapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_route_id">>, <<"syneverse">>)).
-define(DEFAULT_FROM,
        kapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_from_number">>, <<"15552220001">>)).

-spec send_outbound_sms(ne_binary(), ne_binary()) -> 'ok'.
-spec send_outbound_sms(ne_binary(), ne_binary(), pos_integer()) -> 'ok'.
-spec send_outbound_sms(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_outbound_sms(To, Msg) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg).

send_outbound_sms(To, Msg, Times) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg, Times).

send_outbound_sms(To, From, RouteId, Msg) ->
    Payload = [{<<"Message-ID">>, kz_util:rand_hex_binary(16)}
              ,{<<"System-ID">>, kz_util:node_name()}
              ,{<<"Route-ID">>, RouteId}
              ,{<<"From">>, From}
              ,{<<"To">>, kz_util:to_binary(To)}
              ,{<<"Body">>, Msg}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_sms:publish_outbound/1).

-spec send_outbound_sms(ne_binary(), ne_binary(), ne_binary(), ne_binary(), pos_integer()) -> 'ok'.
send_outbound_sms(To, From, RouteId, Msg, Times) ->
    F = fun (X) ->
                MSG = <<"MSG - ", (kz_util:to_binary(X))/binary, " => ", Msg/binary>>,
                send_outbound_sms(To, From, RouteId, MSG),
                timer:sleep(2000)
        end,
    lists:foreach(F, lists:seq(1, kz_util:to_integer(Times))).
