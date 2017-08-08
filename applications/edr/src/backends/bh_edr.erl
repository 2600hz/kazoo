%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Max Lay
%%%-------------------------------------------------------------------
-module(bh_edr).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole/src/blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.edr">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.edr">>, ?MODULE, 'bindings').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>]}) ->
    Context;
validate(Context, #{keys := [Verbosity]}) ->
    EdrLevels = [kz_term:to_binary(L) || L <- ?EDR_VERBOSITY_LEVELS],
    case lists:member(Verbosity, EdrLevels) of
        'true' ->
            Context;
        'false' ->
            bh_context:add_error(Context, <<"edr verbosity must be one of: ", (kz_binary:join(EdrLevels))/binary>>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for edr subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [Verbosity]
                    }=Map) ->
    Requested = <<"edr_blackhole.", Verbosity/binary>>,
    Subscribed = [<<"edr_blackhole.", "*.", Verbosity/binary, ".", AccountId/binary>>],
    Listeners = [{'amqp', 'edr_blackhole', edr_bind_options(AccountId, Verbosity)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec edr_bind_options(ne_binary(), ne_binary()) -> gen_listener:bindings().
edr_bind_options(AccountId, Verbosity) ->
    [{'verbosity', Verbosity}
    ,{'account_id', AccountId}
    ,'federate'
    ].
