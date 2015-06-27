%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
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
    wh_cache:flush_local(?DOODLE_CACHE).

-spec start_check_sms_by_device_id(ne_binary(), ne_binary()) -> pid().
start_check_sms_by_device_id(AccountId, DeviceId) ->
    wh_util:spawn(?MODULE, 'check_sms_by_device_id', [AccountId, DeviceId]).

-spec start_check_sms_by_owner_id(ne_binary(), ne_binary()) -> pid().
start_check_sms_by_owner_id(AccountId, OwnerId) ->
    wh_util:spawn(?MODULE, 'check_sms_by_owner_id', [AccountId, OwnerId]).

-spec check_sms_by_device_id(ne_binary(), ne_binary()) -> 'ok'.
check_sms_by_device_id(_AccountId, 'undefined') -> 'ok';
check_sms_by_device_id(AccountId, DeviceId) ->
    ViewOptions = [{'endkey', [DeviceId, wh_util:current_tstamp()]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_device">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms by device for ~s/~s: ~p", [AccountId, DeviceId, _R])
    end.

-spec check_sms_by_owner_id(ne_binary(), api_binary()) -> 'ok'.
check_sms_by_owner_id(_AccountId, 'undefined') -> 'ok';
check_sms_by_owner_id(AccountId, OwnerId) ->
    ViewOptions = [{'endkey', [OwnerId, wh_util:current_tstamp()]}],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_owner">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms by owner_id for owner_id ~s in account ~s: ~p", [AccountId, OwnerId, _R])
    end.

-spec start_check_sms_by_account(ne_binary(), wh_json:object()) -> pid().
start_check_sms_by_account(AccountId, JObj) ->
     case wh_doc:is_soft_deleted(JObj)
         orelse wh_json:is_false(<<"enabled">>, JObj, 'true')
     of
         'true' -> 'ok';
         'false' -> wh_util:spawn(?MODULE, 'check_pending_sms_for_delivery', [AccountId])
     end.

-spec check_pending_sms_for_outbound_delivery(ne_binary()) -> pid().
check_pending_sms_for_outbound_delivery(AccountId) ->
    wh_util:spawn(fun() -> check_pending_sms_for_offnet_delivery(AccountId) end),
    wh_util:spawn(fun() -> check_queued_sms(AccountId) end).

-spec check_pending_sms_for_delivery(ne_binary()) -> 'ok'.
check_pending_sms_for_delivery(AccountId) ->
    ViewOptions = [{'limit', 100}
                  ,{'endkey', wh_util:current_tstamp()}
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

-spec replay_queue_sms(ne_binary(), wh_json:objects()) -> 'ok'.
replay_queue_sms(AccountId, JObjs) ->
    lager:debug("starting queued sms for account ~s", [AccountId]),
    _ = [spawn_handler(AccountId, JObj)
         || JObj <- JObjs
        ],
    'ok'.

-spec spawn_handler(ne_binary(), wh_json:object()) -> 'ok'.
spawn_handler(AccountId, JObj) ->
    DocId = wh_json:get_value(<<"id">>, JObj),
    <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId,
    AccountDb = kazoo_modb:get_modb(AccountId, Year, Month),

    _ = wh_util:spawn('doodle_api', 'handle_api_sms', [AccountDb, DocId]),
    timer:sleep(200).

-spec check_pending_sms_for_offnet_delivery(ne_binary()) -> 'ok'.
check_pending_sms_for_offnet_delivery(AccountId) ->
    ViewOptions = [{'limit', 100}
                   ,{'endkey', wh_util:current_tstamp()}
                  ],
    case kazoo_modb:get_results(AccountId, <<"sms/deliver_to_offnet">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} -> replay_sms(AccountId, JObjs);
        {'error', _R} ->
            lager:debug("unable to get sms list for offnet delivery in account ~s : ~p", [AccountId, _R])
    end.

-spec replay_sms(ne_binary(), wh_json:objects()) -> 'ok'.
replay_sms(AccountId, JObjs) ->
    lager:debug("starting sms offnet delivery for account ~s", [AccountId]),
    _ = [begin
             doodle_util:replay_sms(AccountId, wh_json:get_value(<<"id">>, JObj)),
             timer:sleep(200)
         end
         || JObj <- JObjs
        ],
    'ok'.

-define(DEFAULT_ROUTEID, whapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_route_id">>, <<"syneverse">>)).
-define(DEFAULT_FROM, whapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_from_number">>, <<"15552220001">>)).

send_outbound_sms(To, Msg) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg).

send_outbound_sms(To, Msg, Times) ->
    send_outbound_sms(To, ?DEFAULT_FROM, ?DEFAULT_ROUTEID, Msg, Times).

send_outbound_sms(To, From, RouteId, Msg) ->
    Payload = [{<<"Message-ID">>, wh_util:rand_hex_binary(16)}
               ,{<<"System-ID">>, wh_util:node_name()}
               ,{<<"Route-ID">>, RouteId}
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
     || X <- lists:seq(1, wh_util:to_integer(Times))
    ].
