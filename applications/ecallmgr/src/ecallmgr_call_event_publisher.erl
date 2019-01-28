%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_call_event_publisher).


-export([init/0]).

-export([publish_call_event/1
        ]).


-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.publish.call_event.*">>, ?MODULE, 'publish_call_event'),
    'ok'.

-spec publish_call_event(tuple()) -> any().
publish_call_event({_Node, _UUID, _Category, _Event, JObj}) ->
    kz_util:put_callid(JObj),
    kapi_call:publish_event(JObj).
