%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Peter Defebvre
%%% @author Ben Wann
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


-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.call">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.call">>, ?MODULE, 'bindings').


-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, _]
                   }) ->
    Context;
validate(Context, #{keys := [Event, _]
                   }) ->
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
    Requested = <<"call.*.", CallId/binary>>,
    Subscribed = [<<"call.", AccountId/binary, ".", Event/binary, ".", CallId/binary>>
                      || Event <- ?LISTEN_TO],
    Listeners = [{'hook', AccountId, Event} || Event <- ?LISTEN_TO],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [Event, CallId]
                    }=Map) ->
    Requested = <<"call.", Event/binary, ".", CallId/binary>>,
    Subscribed = [<<"call.", AccountId/binary, ".", Event/binary, ".", CallId/binary>>],
    Listeners = [{'hook', AccountId, Event}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.
