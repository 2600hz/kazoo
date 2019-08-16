%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Listens for a list of events and gproc-sends them out to folks who
%%% want them
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call_events).

-export([start_link/0
        ,handle_event/1
        ,is_destroyed/1
        ,get_event/1
        ]).

-include("kapps_call_command.hrl").

-define(EXPIRES_S, 30).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = kz_hooks:bind(<<"*">>, <<"CHANNEL_DESTROY">>, {?MODULE, 'handle_event', []}),
    'ignore'.

-spec handle_event(kz_call_event:doc()) -> 'ok'.
handle_event(EventJObj) ->
    kz_cache:store_local(?KAPPS_CALL_CACHE, {?MODULE, kz_call_event:call_id(EventJObj)}, 'true', [{'expires', ?EXPIRES_S}]).

-spec is_destroyed(kz_term:ne_binary() | kapps_call:call()) -> boolean().
is_destroyed(<<CallId/binary>>) ->
    {'error', 'not_found'} =/= get_event(CallId);
is_destroyed(Call) ->
    is_destroyed(kapps_call:call_id(Call)).

-spec get_event(kz_term:ne_binary() | kapps_call:call()) -> {'ok', kz_call_event:doc()} |
                                                            {'error', 'not_found'}.
get_event(<<CallId/binary>>) ->
    kz_cache:peek_local(?KAPPS_CALL_CACHE, {?MODULE, CallId});
get_event(Call) ->
    get_event(Call).
