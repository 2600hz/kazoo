%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Peter Defebvre
%%%   Ben Wann
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_conference).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.conference">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.conference">>, ?MODULE, 'bindings').


-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"command">>, <<"*">>]
                   }) ->
    bh_context:add_error(Context, <<"ConferenceId required">>);
validate(Context, #{keys := [<<"command">>, _]
                   }) ->
    Context;
validate(Context, #{keys := [<<"event">>, <<"*">>, _]
                   }) ->
    bh_context:add_error(Context, <<"ConferenceId required">>);
validate(Context, #{keys := [<<"event">>, _, _]
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for conference subscription : ", (kz_util:join_binary(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := _AccountId
                    ,keys := [<<"command">>, ConferenceId]
                    }=Map) ->
    Requested = <<"conference.command.", ConferenceId/binary>>,
    Subscribed = [<<"conference.command.", ConferenceId/binary>>],
    Listeners = [{'amqp', 'conference', command_binding_options(ConferenceId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := _AccountId
                    ,keys := [<<"event">>, ConferenceId, CallId]
                    }=Map) ->
    Requested = <<"conference.event.", ConferenceId/binary, ".", CallId/binary>>,
    Subscribed = [<<"conference.event.", ConferenceId/binary, ".", CallId/binary>>],
    Listeners = [{'amqp', 'conference', event_binding_options(ConferenceId, CallId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

%%%===================================================================
%%% Internal functions
%%%==================================================================

-spec command_binding_options(ne_binary()) -> kz_proplist().
command_binding_options(ConfId) ->
    [{'restrict_to', [{'command', ConfId}]}
    ,'federate'
    ].

-spec event_binding_options(ne_binary(), ne_binary()) -> kz_proplist().
event_binding_options(ConfId, CallId) ->
    [{'restrict_to', [{'event', {ConfId, CallId}}]}
    ,'federate'
    ].
