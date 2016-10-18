%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Maintenance functions for all
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnipresence_maintenance).

-include("omnipresence.hrl").

-export([current_subscriptions/0, current_subscriptions/1, current_subscriptions/2
        ,count_current_subscriptions/0
        ,subscribe/2
        ,send_mwi_update/3
        ,list_terminated_callids/0
        ,reset_subscription/1, reset_subscription/2
        ,reset_subscriber/1, reset_subscriber/2
        ]).

-spec count_current_subscriptions() -> 'no_return'.
count_current_subscriptions() ->
    CurrentSubscriptions = ets:tab2list(omnip_subscriptions:table_id()),
    io:format("~p\n", [length(CurrentSubscriptions)]),
    'no_return'.

-define(SUBSCRIPTION_FORMAT_STR, " ~50.s | ~50.s | ~10.s | ~20.s |~n").

-spec current_subscriptions() -> 'ok'.
-spec current_subscriptions(ne_binary()) -> 'ok'.
-spec current_subscriptions(ne_binary(), ne_binary()) -> 'ok'.
current_subscriptions() ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        ets:tab2list(omnip_subscriptions:table_id())
       )).

current_subscriptions(Realm) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions('_', kz_util:to_binary(Realm))
       )).

current_subscriptions(Realm, User) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions(
          '_', kz_util:to_binary(Realm), kz_util:to_binary(User)
         ))).

print_subscriptions([]) ->
    io:format("No subscriptions have been found~n");
print_subscriptions(Ss) ->
    Now = kz_util:current_tstamp(),
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
              ,kz_util:to_binary(ExpiresIn)
              ,kz_json:get_value(<<"event">>, JObj)
              ]).

-spec subscribe(ne_binary(), ne_binary()) -> 'ok'.
subscribe(Realm, User) ->
    Prop = [{<<"User">>, <<"sip:", User/binary, "@", Realm/binary>>}
           ,{<<"Expires">>, 1}
           ,{<<"Queue">>, <<>>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    case kapps_util:amqp_pool_request_custom(Prop
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

-spec send_mwi_update(ne_binary(), ne_binary() | integer(), ne_binary() | integer() ) -> 'ok'.
send_mwi_update(User, New, Saved) when is_binary(New) ->
    send_mwi_update(User, kz_util:to_integer(New), Saved);
send_mwi_update(User, New, Saved) when is_binary(Saved) ->
    send_mwi_update(User, New, kz_util:to_integer(Saved));
send_mwi_update(User, New, Saved) ->
    Command = [{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Call-ID">>, kz_util:rand_hex_binary(16) }
              ,{<<"To">>, User}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kapps_util:amqp_pool_send(Command, fun kapi_presence:publish_mwi_update/1),
    'ok'.

-spec list_terminated_callids() -> 'ok'.
list_terminated_callids() ->
    io:format("Here are the call IDs currently cached as terminated:~n", []),
    io:format("~s~n", [kz_util:join_binary(
                         omnip_subscriptions:cached_terminated_callids()
                                          ,<<", ">>
                        )
                      ]).

-spec reset_subscription(ne_binary()) -> any().
reset_subscription(User) ->
    [Username, Realm | _] = binary:split(User, <<"@">>, ['global']),
    reset_subscription(Username, Realm).

-spec reset_subscription(ne_binary(), ne_binary()) -> any().
reset_subscription(User, Realm) ->
    JObj = kz_json:from_list(
             [{<<"Realm">>, Realm}
             ,{<<"Username">>, User}
             ]),
    omnip_subscriptions:reset(JObj).

-spec reset_subscriber(ne_binary()) -> any().
reset_subscriber(User) ->
    case omnip_subscriptions:find_user_subscriptions(?OMNIPRESENCE_EVENT_ALL, User) of
        {'ok', Subs} ->
            List = [Username || #omnip_subscription{user=Username} <- Subs],
            [reset_subscription(Sub) || Sub <- lists:usort(List)];
        _ -> 'ok'
    end.

-spec reset_subscriber(ne_binary(), ne_binary()) -> any().
reset_subscriber(User, Realm) ->
    reset_subscriber(<<User/binary, "@", Realm/binary>>).
