%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_event_stream_notify_gproc).


-export([init/0]).

-export([notify_event/1
        ]).


-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.notify.call_event.*">>, ?MODULE, 'notify_event'),
    'ok'.

-spec notify_event(map()) -> any().
notify_event(#{node := Node, call_id := UUID, event := Event, payload := JObj}) ->
    kz_util:put_callid(JObj),
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, Event)}, {'event', UUID , JObj}),
    maybe_send_call_event(UUID, Event, JObj, Node).

-spec maybe_send_call_event(kz_term:api_binary(), kz_term:ne_binary(), kz_json:object(), atom()) -> any().
maybe_send_call_event('undefined', _, _, _) -> 'ok';
maybe_send_call_event(CallId, Event, JObj, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_MSG(Node, Event, CallId)}, {'event', Event, CallId, JObj}),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', CallId, JObj}).
