%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Peter Defebvre
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_call).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(LISTEN_TO, [<<"CHANNEL_CREATE">>, <<"CHANNEL_ANSWER">>, <<"CHANNEL_DESTROY">>
                   ,<<"CHANNEL_HOLD">>, <<"CHANNEL_UNHOLD">>
                   ,<<"CHANNEL_BRIDGE">>
                   ,<<"PARK_PARKED">>, <<"PARK_RETRIEVED">>, <<"PARK_ABANDONED">>
                   ]).

-define(ACCOUNT_BINDING(AccountId, Event, CallId)
       ,<<"call.", AccountId/binary, ".", Event/binary, ".", CallId/binary>>
       ).
-define(BINDING(Event, CallId), <<"call.", Event/binary, ".", CallId/binary>>).
-define(BINDING(Event), ?BINDING(Event, <<"*">>)).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.call">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.call">>, ?MODULE, 'bindings').

-spec init_bindings() -> 'ok'.
init_bindings() ->
    Bindings = [?BINDING(Event, <<"{CALL_ID}">>) || Event <- ?LISTEN_TO],
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"call">>], Bindings) of
        {'ok', _} -> lager:debug("initialized call bindings");
        {'error', _E} -> lager:info("failed to initialize call bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, _]}) ->
    Context;
validate(Context, #{keys := [Event, _]}) ->
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, " not supported">>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for call subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"*">>, CallId]
                    }=Map) ->
    Requested = ?BINDING(<<"*">>, CallId),
    Subscribed = [?ACCOUNT_BINDING(AccountId, Event, CallId)
                  || Event <- ?LISTEN_TO
                 ],
    Listeners = [{'hook', AccountId, Event} || Event <- ?LISTEN_TO],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [Event, CallId]
                    }=Map) ->
    Requested = ?BINDING(Event, CallId),
    Subscribed = [?ACCOUNT_BINDING(AccountId, Event, CallId)],
    Listeners = [{'hook', AccountId, Event}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.
