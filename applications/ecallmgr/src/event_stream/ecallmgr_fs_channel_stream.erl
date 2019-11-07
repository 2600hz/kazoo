%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,channel_update/1
        ]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_CREATE">>, ?MODULE, 'channel_create'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DESTROY">>, ?MODULE, 'channel_destroy'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_ANSWER">>, ?MODULE, 'channel_answer'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_BRIDGE">>, ?MODULE, 'channel_bridge'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNBRIDGE">>, ?MODULE, 'channel_unbridge'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_HOLD">>, ?MODULE, 'channel_hold'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNHOLD">>, ?MODULE, 'channel_unhold'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DATA">>, ?MODULE, 'channel_data'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CALL_UPDATE">>, ?MODULE, 'channel_update'),
    _ = kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_SYNC">>, ?MODULE, 'channel_sync'),
    'ok'.

-spec channel_create(map()) -> any().
channel_create(#{node := Node, call_id := UUID, payload := JObj}) ->
    _ = maybe_authorize_channel(Node, UUID, JObj),
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

-spec channel_update(map()) -> any().
channel_update(#{node := Node, call_id := UUID, payload := JObj}) ->
    ecallmgr_fs_channel:update(Node, UUID, JObj).

-spec channel_sync(map()) -> any().
channel_sync(#{node := Node, call_id := UUID, payload := JObj}) ->
    EventNode = kz_json:get_ne_binary_value(<<"Event-Node">>, JObj),
    case EventNode =:= kz_term:to_binary(node())
        andalso kz_json:get_ne_binary_value(<<"Event-PID">>, JObj)
    of
        'false' -> lager:warning("sync not for this node ~s / ~s", [Node, EventNode]);
        'undefined' -> ecallmgr_fs_channel:update(Node, UUID, JObj);
        Pid -> kz_term:to_pid(Pid) ! {'channel_sync', JObj}
    end.

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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
maybe_authorize_channel(Node, UUID, JObj) ->
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Ecallmgr-Node">>], JObj) =:= kz_term:to_binary(node()) of
        'true' -> kz_process:spawn(fun authorize_channel/3, [Node, UUID, JObj]);
        'false' -> 'ok'
    end.

authorize_channel(Node, UUID, JObj) ->
    case ecallmgr_fs_authz:authorize(JObj, UUID, Node) of
        {'true', CCVs} -> ecallmgr_fs_command:set(Node, UUID, kz_json:to_proplist(CCVs));
        Authz -> Authz
    end.
