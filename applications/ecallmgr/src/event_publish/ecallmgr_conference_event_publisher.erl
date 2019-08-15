%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Execute conference commands
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_conference_event_publisher).

%% API
-export([init/0
        ,publish_event/1
        ]).

-include("ecallmgr.hrl").


-define(MEMBER_UPDATE_EVENTS, [<<"stop-talking">>
                              ,<<"start-talking">>
                              ,<<"mute-member">>
                              ,<<"unmute-member">>
                              ,<<"deaf-member">>
                              ,<<"undeaf-member">>
                              ]).

-define(CONFERENCE_EVENTS, [<<"conference-create">>
                           ,<<"conference-destroy">>
                           ,<<"lock">>
                           ,<<"unlock">>
                           ,<<"add-member">>
                           ,<<"del-member">>
                                | ?MEMBER_UPDATE_EVENTS
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
    _ = kazoo_bindings:bind(<<"event_stream.publish.conference.event">>, ?MODULE, 'publish_event'),
    'ok'.

-spec publish_event(map()) -> 'ok'.
publish_event(#{payload := JObj}) ->
    Event = kz_conference_event:event(JObj),
    case lists:member(Event, events())
        andalso kz_conference_event:conference_node(JObj) =:= kz_term:to_binary(node())
    of
        'true' -> kapi_conference:publish_event(JObj);
        'false' -> lager:debug("not publishing conference event : ~s", [Event])
    end.

events() ->
    kapps_config:get_ne_binaries(?APP_NAME, <<"publish_conference_event">>, ?CONFERENCE_EVENTS).
