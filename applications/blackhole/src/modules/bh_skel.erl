%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
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

-spec skel_bind_options(ne_binary(), ne_binary()) -> kz_proplist().
skel_bind_options(AccountId, MyId) ->
    [{'restrict_to', ['skel.updates']}
    ,{'account_id', AccountId}
    ,{'skel_id', MyId}
    ,'federate'
    ].
