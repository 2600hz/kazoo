%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_event_publisher).


-export([init/0]).

-export([publish_call_event/1
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
    kazoo_bindings:bind(<<"event_stream.publish.call_event.*">>, ?MODULE, 'publish_call_event'),
    'ok'.

-spec publish_call_event(map()) -> any().
publish_call_event(#{payload := JObj}) ->
    kz_util:put_callid(JObj),
    Node = kz_term:to_binary(node()),
    case ?RESTRICTED_PUBLISHING
        andalso kz_call_event:custom_channel_var(JObj, <<"Ecallmgr-Node">>)
    of
        'false' -> kapi_call:publish_event(JObj);
        'undefined' -> kapi_call:publish_event(JObj);
        Node -> kapi_call:publish_event(JObj);
        _Other -> 'ok'
    end.
