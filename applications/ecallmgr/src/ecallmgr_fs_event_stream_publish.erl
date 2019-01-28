%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_event_stream_publish).


-export([init/0]).

-export([publish_call_event/1]).


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
    kazoo_bindings:bind(<<"event_stream.publish.call_event.*">>, ?MODULE, 'publish_call_event'),
    'ok'.

-spec publish_call_event(map()) -> any().
publish_call_event(#{payload := JObj}) ->
    kz_util:put_callid(JObj),
    kapi_call:publish_event(JObj).
