%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_presence).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(LISTEN_TO, [<<"dialog">>, <<"mwi_updates">>, <<"update">>]).
-define(BINDING(Event, Arg)
       ,<<Event/binary, ".", Arg/binary>>
       ).
-define(REALM_BINDING(Event, Realm, Arg)
       ,<<Event/binary, ".", (kz_amqp_util:encode(Realm))/binary, ".", Arg/binary>>
       ).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.presence">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.presence">>, ?MODULE, 'bindings').

-spec init_bindings() -> 'ok'.
init_bindings() ->
    Bindings = [?BINDING(<<"dialog">>, <<"{CALL_ID}">>)
               ,?BINDING(<<"mwi_updates">>, <<"{USER}">>)
               ,?BINDING(<<"update">>, <<"{CALL_ID}">>)],
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"presence">>], Bindings) of
        {'ok', _} -> lager:debug("initialized presence bindings");
        {'error', _E} -> lager:info("failed to initialize presence bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [Event | _]}) ->
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, " not supported">>)
    end;
validate(Context, #{'keys' := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for presence subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"dialog">>=Event, CallId]
                    }=Map) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    Requested = ?BINDING(Event, CallId),
    Subscribed = [?REALM_BINDING(Event, Realm, CallId)],
    Listeners = [{'amqp', 'presence', dialog_bind_options(Realm, CallId)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        };
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"mwi_updates">>=Event, User]
                    }=Map) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    Requested = ?BINDING(Event, User),
    Subscribed = [?REALM_BINDING(Event, Realm, User)],
    Listeners = [{'amqp', 'presence', mwi_bind_options(Realm, User)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        };
bindings(_Context, #{'account_id' := AccountId
                    ,'keys' := [<<"update">>=Event, CallId]
                    }=Map) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    Requested = ?BINDING(Event, CallId),
    Subscribed = [?REALM_BINDING(Event, Realm, CallId)],
    Listeners = [{'amqp', 'presence', update_bind_options(Realm, CallId)}],
    Map#{'requested' => Requested
        ,'subscribed' => Subscribed
        ,'listeners' => Listeners
        }.

-spec dialog_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
dialog_bind_options(PresenceId, CallId) ->
    [{'restrict_to', ['dialog']}
    ,{'presence-id', PresenceId}
    ,{'call', CallId}
    ,'federate'
    ].

-spec mwi_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
mwi_bind_options(Realm, User) ->
    [{'restrict_to', ['mwi_update']}
    ,{'realm', Realm}
    ,{'user', User}
    ,'federate'
    ].

-spec update_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
update_bind_options(PresenceId, CallId) ->
    [{'restrict_to', ['update']}
    ,{'presence-id', PresenceId}
    ,{'call', CallId}
    ,'federate'
    ].
