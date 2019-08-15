%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Peter Defebvre
%%% @author Ben Wann
%%% @author Roman Galeev
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_conference).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(ACCOUNT_BINDING(AccountId, ConferenceId, CallId)
       ,<<"conference.event.*.", AccountId/binary, ".", ConferenceId/binary, ".", CallId/binary>>
       ).
-define(BINDING(ConferenceId, CallId)
       ,<<"conference.event.", ConferenceId/binary, ".", CallId/binary>>
       ).
-define(COMMAND(ConferenceId)
       ,<<"conference.command.", ConferenceId/binary>>
       ).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.conference">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.conference">>, ?MODULE, 'bindings').

init_bindings() ->
    Bindings = [?BINDING(<<"{CONFERENCE_ID}">>, <<"{CALL_ID}">>)
               ,?COMMAND(<<"{CONFERENCE_ID}">>)
               ],
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"conference">>], Bindings) of
        {'ok', _} -> lager:debug("initialized conference bindings");
        {'error', _E} -> lager:info("failed to initialize conference bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"command">>, <<"*">>]}) ->
    bh_context:add_error(Context, <<"ConferenceId required">>);
validate(Context, #{keys := [<<"command">>, _]}) ->
    Context;
validate(Context, #{keys := [<<"event">>, <<"*">>, _]}) ->
    bh_context:add_error(Context, <<"ConferenceId required">>);
validate(Context, #{keys := [<<"event">>, _, _]}) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for conference subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := _AccountId
                    ,keys := [<<"command">>, ConferenceId]
                    }=Map) ->
    Requested = ?COMMAND(ConferenceId),
    Subscribed = [?COMMAND(ConferenceId)],
    Listeners = [{'amqp', 'conference', command_binding_options(ConferenceId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"event">>, ConferenceId, CallId]
                    }=Map) ->
    Requested = ?BINDING(ConferenceId, CallId),
    Subscribed = [?ACCOUNT_BINDING(AccountId, ConferenceId, CallId)],
    Listeners = [{'amqp', 'conference', event_binding_options(AccountId, ConferenceId, CallId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec command_binding_options(kz_term:ne_binary()) -> kz_term:proplist().
command_binding_options(ConfId) ->
    [{'restrict_to', [{'command', ConfId}]}
    ,'federate'
    ].

-spec event_binding_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
event_binding_options(AccountId, ConferenceId, CallId) ->
    [{'restrict_to', [{'event', [{'account_id', AccountId}
                                ,{'conference_id', ConferenceId}
                                ,{'call_id', CallId}
                                ]
                      }]
     }
    ,'federate'
    ].
