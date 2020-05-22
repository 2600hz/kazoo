%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, Kage DS Ltd
%%% @doc
%%% @author Alan R Evans
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_acdc_agent).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include_lib("../blackhole/src/blackhole.hrl").

-define(BINDING(),
        [
         <<"acdc.agent.action.*">>
        ,<<"acdc.agent.change.*">>
        ,<<"acdc_stats.status.*">>
        ]).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc">>, ?MODULE, 'validate'),
    _ = blackhole_bindings:bind(<<"blackhole.events.bindings.acdc">>, ?MODULE, 'bindings'),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc_stats">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.acdc_stats">>, ?MODULE, 'bindings').

init_bindings() ->
    Bindings = ?BINDING(),
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"agent">>], Bindings) of
        {'ok', _} -> lager:debug("initialized ACDC agent bindings");
        {'error', _E} -> lager:info("failed to initialize ACDC agent bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"agent">>,<<"action">>,<<"*">>]
                   }) ->
    Context;
validate(Context, #{keys := [<<"agent_change">>,<<"*">>]
                   }) ->
    Context;
validate(Context, #{keys := [<<"status">>, _AccountId, _AgentId]
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for agents subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"agent">>, <<"action">>, Action]
                    }=Map) ->
    Requested = <<"acdc.agent.action.", Action/binary>>,
    Subscribed = [<<"acdc.agent.action.", Action/binary, ".", AccountId/binary, ".*">>],
    Listeners = [{'amqp', 'acdc_agent', bind_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"agent_change">>, QueueId]
                    }=Map) ->
    Requested = <<"acdc.agent.change.", QueueId/binary>>,
    Subscribed = [<<"acdc.queue.agent_change.", AccountId/binary, ".", QueueId/binary>>],
    Listeners = [{'amqp', 'acdc_queue', bind_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"status">>, AccountId, AgentId]
                    }=Map) ->
    Requested = <<"acdc_stats.status.", AccountId/binary, ".", AgentId/binary>>,
    Subscribed = [<<"acdc_stats.status.", AccountId/binary, ".", AgentId/binary>>],
    Listeners = [{'amqp', 'acdc_stats', bind_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec bind_options(kz_term:ne_binary()) -> kz_term:proplist().
bind_options(AccountId) ->
    [
     {'account_id', AccountId}
    ,'federate'
    ].
