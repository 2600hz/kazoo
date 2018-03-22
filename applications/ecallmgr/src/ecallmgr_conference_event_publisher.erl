%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Execute conference commands
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
    kazoo_bindings:bind(<<"event_stream.publish.conference.event">>, ?MODULE, 'publish_event'),
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
    ecallmgr_config:get_ne_binaries(<<"publish_conference_event">>, ?CONFERENCE_EVENTS).
