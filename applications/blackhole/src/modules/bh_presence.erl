%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_presence).

%% "dialog.*.*", "update.*.*", "mwi_updates.*.*"

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(LISTEN_TO, [<<"dialog">>, <<"mwi_updates">>, <<"update">>]).
-define(BINDING(Event, Arg1, Arg2)
       ,<<Event/binary, ".", Arg1/binary, ".", Arg2/binary>>
       ).

-spec init() -> any().
init() ->
    lager:notice("bh_presence init/0 loaded."),
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.presence">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.presence">>, ?MODULE, 'bindings').

-spec init_bindings() -> 'ok'.
init_bindings() ->
    Bindings = [?BINDING(<<"dialog">>, <<"{PRESENCE_ID}">>, <<"{CALL_ID}">>)
               ,?BINDING(<<"update">>, <<"{PRESENCE_ID}">>, <<"{CALL_ID}">>)
               ,?BINDING(<<"mwi_updates">>,  <<"{REALM}">>, <<"{USER}">>)],
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"presence">>], Bindings) of
        {'ok', _} -> lager:debug("initialized presence bindings");
        {'error', _E} -> lager:info("failed to initialize presence bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, _]}) ->
    Context;
validate(Context, #{keys := [Event | _]}) ->
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, " not supported">>)
    end;
validate(Context, #{'keys' := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for presence subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"dialog">>=Event, PresenceId, CallId]
                    }=Map) ->
    Requested = ?BINDING(Event,  PresenceId, CallId),
    Subscribed = [?BINDING(Event, PresenceId, CallId)],
    Listeners = [{'amqp', 'presence', dialog_bind_options(AccountId, PresenceId, CallId)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        };
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"mwi_updates">>=Event, Realm, User]
                    }=Map) ->
    Requested = ?BINDING(Event,  Realm, User),
    Subscribed = [?BINDING(Event, Realm, User)],
    Listeners = [{'amqp', 'presence', mwi_bind_options(AccountId, Realm, User)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        };
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"update">>=Event, PresenceId, CallId]
                    }=Map) ->
    Requested = ?BINDING(Event, PresenceId, CallId),
    Subscribed = [?BINDING(Event, PresenceId, CallId)],
    Listeners = [{'amqp', 'presence', update_bind_options(AccountId, PresenceId, CallId)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        }.

-spec dialog_bind_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
dialog_bind_options(AccountId,  PresenceId, CallId) ->
    [{'restrict_to', ['dialog']}
    ,{'account_id',  AccountId}
    ,{'presence-id', PresenceId}
    ,{'call', CallId}
    ,'federate'
    ].

-spec mwi_bind_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
mwi_bind_options(AccountId, Realm, User) ->
    [{'restrict_to', ['mwi_update']}
    ,{'account_id',  AccountId}
    ,{'realm', Realm}
    ,{'user', User}
    ,'federate'
    ].

-spec update_bind_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
update_bind_options(AccountId, PresenceId, CallId) ->
    [{'restrict_to', ['update']}
    ,{'account_id',  AccountId}
    ,{'presence-id', PresenceId}
    ,{'call', CallId}
    ,'federate'
    ].


