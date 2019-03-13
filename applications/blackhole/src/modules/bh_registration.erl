%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019, Voxter Communications Inc.
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_registration).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(BINDING(Realm)
       ,<<"registration.success.", (kz_amqp_util:encode(Realm))/binary, ".*">>
       ).

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.registration">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.registration">>, ?MODULE, 'bindings').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"success">>]}) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for registration subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"success">>]
                    }=Map) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    Requested = <<"registration.success">>,
    Subscribed = [?BINDING(Realm)],
    Listeners = [{'amqp', 'registration', registration_bind_options(Realm)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec registration_bind_options(kz_term:ne_binary()) -> kz_term:proplist().
registration_bind_options(Realm) ->
    [{'restrict_to', ['reg_success']}
    ,{'realm', Realm}
    ,'federate'
    ].
