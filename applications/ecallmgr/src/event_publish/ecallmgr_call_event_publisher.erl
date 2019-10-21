%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_event_publisher).


-export([init/0]).

-export([publish_call_event/1
        ]).


-include("ecallmgr.hrl").

-define(EXCLUDE_PUBLISH_EVENTS, [<<"CHANNEL_DATA">>
                                ,<<"CALL_UPDATE">>
                                ,<<"PRESENCE_IN">>
                                ]).
%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.publish.call_event.*">>, ?MODULE, 'publish_call_event'),
    'ok'.

-spec publish_call_event(map()) -> any().
publish_call_event(#{payload := JObj}=Map) ->
    kz_log:put_callid(JObj),
    EventName = kz_api:event_name(JObj),
    case lists:member(EventName, ?EXCLUDE_PUBLISH_EVENTS) of
        'true' -> 'ok';
        'false' -> do_publish_call_event(Map)
    end.

-spec do_publish_call_event(map()) -> any().
do_publish_call_event(#{payload := JObj}) ->
    Node = kz_term:to_binary(node()),
    case ?RESTRICTED_PUBLISHING
        andalso kz_evt_freeswitch:ccv(JObj, <<"Ecallmgr-Node">>)
    of
        'false' -> kapi_call:publish_event(JObj);
        'undefined' ->
            lager:debug("publishing ~s with restricted and undefined", [kz_api:event_name(JObj)]),
            kapi_call:publish_event(JObj);
        Node -> kapi_call:publish_event(JObj);
        _Other -> lager:debug("not publishing ~s", [kz_api:event_name(JObj)])
    end.
