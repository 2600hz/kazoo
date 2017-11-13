%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_channel_stream).

-export([init/0]).

-export([channel_create/1
        ,channel_destroy/1
        ,channel_answer/1
        ,channel_bridge/1
        ,channel_unbridge/1
        ,channel_data/1
        ,channel_sync/1
        ,channel_hold/1, channel_unhold/1
        ]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_CREATE">>, ?MODULE, 'channel_create'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DESTROY">>, ?MODULE, 'channel_destroy'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_ANSWER">>, ?MODULE, 'channel_answer'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_BRIDGE">>, ?MODULE, 'channel_bridge'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNBRIDGE">>, ?MODULE, 'channel_unbridge'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_HOLD">>, ?MODULE, 'channel_hold'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNHOLD">>, ?MODULE, 'channel_unhold'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DATA">>, ?MODULE, 'channel_data'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_SYNC">>, ?MODULE, 'channel_sync'),
    'ok'.

-spec channel_create(map()) -> any().
channel_create(#{node := Node, call_id := UUID, payload := JObj}) ->
    maybe_authorize_channel(Node, UUID, JObj),
    ecallmgr_fs_channel:new(Node, UUID, JObj).

-spec channel_destroy(map()) -> any().
channel_destroy(#{node := Node, call_id := UUID}) ->
    _ = ecallmgr_fs_channels:destroy(UUID, Node).

-spec channel_answer(map()) -> any().
channel_answer(#{call_id := UUID}) ->
    _ = ecallmgr_fs_channels:update(UUID, #channel.answered, 'true').

-spec channel_data(map()) -> any().
channel_data(#{node := Node, call_id := UUID, payload := JObj}) ->
    ecallmgr_fs_channel:update(Node, UUID, JObj).

-spec channel_sync(map()) -> any().
channel_sync(#{node := Node, call_id := UUID, payload := JObj}) ->
    _ = case kz_json:get_ne_binary_value(<<"Event-PID">>, JObj) of
            'undefined' -> 'ok';
            Pid -> kz_term:to_pid(Pid) ! {'channel_sync', JObj}
        end,
    ecallmgr_fs_channel:update(Node, UUID, JObj).

-spec channel_bridge(map()) -> any().
channel_bridge(#{call_id := UUID, payload := JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, OtherLeg),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, UUID).

-spec channel_unbridge(map()) -> any().
channel_unbridge(#{call_id := UUID, payload := JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, 'undefined'),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, 'undefined').

-spec channel_hold(map()) -> any().
channel_hold(#{call_id := UUID}) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'true').

-spec channel_unhold(map()) -> any().
channel_unhold(#{call_id := UUID}) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'false').

%%%===================================================================
%%% Internal functions
%%%===================================================================
maybe_authorize_channel(Node, UUID, JObj) ->
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Ecallmgr-Node">>], JObj) =:= kz_term:to_binary(node()) of
        'true' -> authorize_channel(Node, UUID, JObj);
        'false' -> 'ok'
    end.

authorize_channel(Node, UUID, JObj) ->
    case ecallmgr_fs_authz:authorize(JObj, UUID, Node) of
        {'true', CCVs} -> ecallmgr_fs_command:set(Node, UUID, kz_json:to_proplist(CCVs));
        Authz -> Authz
    end.
