%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Maintenance functions for all
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(omnipresence_maintenance).

-include("omnipresence.hrl").

-export([current_subscriptions/0, current_subscriptions/1, current_subscriptions/2
        ,count_current_subscriptions/0
        ,subscribe/2
        ,send_mwi_update/3
        ,reset_zone/0, reset_zone/1
        ,reset_account/1
        ,reset_subscription/1, reset_subscription/2
        ,reset_subscriber/1, reset_subscriber/2
        ,reset_realm/1
        ,reset_server/1
        ,reset_cluster/0
        ]).

-spec count_current_subscriptions() -> 'no_return'.
count_current_subscriptions() ->
    CurrentSubscriptions = ets:tab2list(omnip_subscriptions:table_id()),
    io:format("~p\n", [length(CurrentSubscriptions)]),
    'no_return'.

-define(SUBSCRIPTION_FORMAT_STR, " ~50.s | ~50.s | ~10.s | ~20.s |~n").

-spec current_subscriptions() -> 'ok'.
current_subscriptions() ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        ets:tab2list(omnip_subscriptions:table_id())
       )).

-spec current_subscriptions(kz_term:ne_binary()) -> 'ok'.
current_subscriptions(Realm) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions('_', kz_term:to_binary(Realm))
       )).

-spec current_subscriptions(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
current_subscriptions(Realm, User) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions(
          '_', kz_term:to_binary(Realm), kz_term:to_binary(User)
         ))).

print_subscriptions([]) ->
    io:format("No subscriptions have been found~n");
print_subscriptions(Ss) ->
    Now = kz_time:now_s(),
    io:format(?SUBSCRIPTION_FORMAT_STR
             ,[<<"Username@Realm">>, <<"From">>, <<"Expires">>, <<"Event">>]
             ),
    F = fun (S) -> print_subscription(S, Now) end,
    lists:foreach(F, Ss).

print_subscription(JObj, Now) ->
    ExpiresIn = kz_json:get_integer_value(<<"expires">>, JObj) -
        (Now - kz_json:get_integer_value(<<"timestamp">>, JObj)),

    io:format(?SUBSCRIPTION_FORMAT_STR
             ,[[kz_json:get_value(<<"username">>, JObj), "@", kz_json:get_value(<<"realm">>, JObj)]
              ,kz_json:get_value(<<"from">>, JObj)
              ,kz_term:to_binary(ExpiresIn)
              ,kz_json:get_value(<<"event">>, JObj)
              ]).

-spec subscribe(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
subscribe(Realm, User) ->
    Prop = [{<<"User">>, <<"sip:", User/binary, "@", Realm/binary>>}
           ,{<<"Expires">>, 1}
           ,{<<"Queue">>, <<>>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    case kz_amqp_worker:call_custom(Prop
                                   ,fun kapi_presence:publish_subscribe/1
                                   ,fun kapi_presence:update_v/1
                                   ,{'presence', [{'restrict_to', ['updates']}]}
                                   )
    of
        {'ok', UpJObj} ->
            io:format("Sent subscription for ~s@~s, recv'd update of state ~s~n"
                     ,[User, Realm, kz_json:get_value(<<"State">>, UpJObj)]
                     );
        {'error', 'timeout'} ->
            io:format("Timed out waiting for update on subscription for ~s@~s~n", [User, Realm]);
        _E ->
            io:format("Sent subscription for ~s@~s, recv'd error: ~p~n", [User, Realm, _E])
    end.

-spec send_mwi_update(kz_term:ne_binary(), kz_term:ne_binary() | integer(), kz_term:ne_binary() | integer() ) -> 'ok'.
send_mwi_update(User, New, Saved) when is_binary(New) ->
    send_mwi_update(User, kz_term:to_integer(New), Saved);
send_mwi_update(User, New, Saved) when is_binary(Saved) ->
    send_mwi_update(User, New, kz_term:to_integer(Saved));
send_mwi_update(User, New, Saved) ->
    Command = [{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Call-ID">>, kz_binary:rand_hex(16) }
              ,{<<"To">>, User}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_mwi_update/1).

-spec reset_subscription(kz_term:ne_binary()) -> any().
reset_subscription(User) ->
    [Username, Realm | _] = binary:split(User, <<"@">>, ['global']),
    reset_subscription(Username, Realm).

-spec reset_subscription(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
reset_subscription(User, Realm) ->
    API = [{<<"Realm">>, Realm}
          ,{<<"Username">>, User}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(API, fun kapi_presence:publish_reset/1).

-spec reset_subscriber(kz_term:ne_binary()) -> any().
reset_subscriber(User) ->
    case omnip_subscriptions:find_user_subscriptions(?OMNIPRESENCE_EVENT_ALL, User) of
        {'ok', Subs} ->
            List = [Username || #omnip_subscription{user=Username} <- Subs],
            [reset_subscription(Sub) || Sub <- lists:usort(List)];
        _ -> 'ok'
    end.

-spec reset_subscriber(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
reset_subscriber(User, Realm) ->
    reset_subscriber(<<User/binary, "@", Realm/binary>>).

-spec reset_account(kz_term:ne_binary()) -> any().
reset_account(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} -> reset_realm(kzd_accounts:realm(JObj));
        {'error', _} = Error -> Error
    end.

-spec reset_realm(kz_term:ne_binary()) -> any().
reset_realm(Realm) ->
    reset_subscription(<<"*">>, Realm).

-spec reset_zone() -> any().
reset_zone() ->
    reset_zone(kz_term:to_binary(kz_config:zone())).

-spec reset_zone(kz_term:ne_binary()) -> any().
reset_zone(Zone) ->
    reset_subscription(Zone, <<"*">>).

-spec reset_server(kz_term:ne_binary()) -> any().
reset_server(Server) ->
    reset_subscription(Server, <<"*">>).

-spec reset_cluster() -> any().
reset_cluster() ->
    reset_subscription(<<"*">>, <<"*">>).
