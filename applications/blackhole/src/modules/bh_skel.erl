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
-module(bh_skel).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.skel">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.skel">>, ?MODULE, 'bindings').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"whatever_comes_after_skel.">>, _]
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for skel subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"myskel">>, MyId]
                    }=Map) ->
    Requested = <<"skel.myskel.", MyId/binary>>,
    Subscribed = [<<"skel.status.", AccountId/binary, ".", MyId/binary>>],
    Listeners = [{'amqp', 'kapi_skel', skel_bind_options(AccountId, MyId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec skel_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
skel_bind_options(AccountId, MyId) ->
    [{'restrict_to', ['skel.updates']}
    ,{'account_id', AccountId}
    ,{'skel_id', MyId}
    ,'federate'
    ].
