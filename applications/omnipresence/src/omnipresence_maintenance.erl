%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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
        ]).

-spec count_current_subscriptions() -> 'no_return'.
count_current_subscriptions() ->
    CurrentSubscriptions = ets:tab2list(omnip_subscriptions:table_id()),
    io:format("~p\n", [length(CurrentSubscriptions)]),
    'no_return'.

-define(SUBSCRIPTION_FORMAT_STR, " ~50.s | ~50.s | ~10.s | ~20.s |~n").

current_subscriptions() ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        ets:tab2list(omnip_subscriptions:table_id())
       )).

current_subscriptions(Realm) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions('_', wh_util:to_binary(Realm))
     )).

current_subscriptions(Realm, User) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions(
          '_', wh_util:to_binary(Realm), wh_util:to_binary(User)
         ))).

print_subscriptions([]) -> io:format("No subscriptions have been found~n");
print_subscriptions(Ss) ->
    Now = wh_util:current_tstamp(),
    io:format(?SUBSCRIPTION_FORMAT_STR
              ,[<<"Username@Realm">>, <<"From">>, <<"Expires">>, <<"Event">>]
             ),
    _ = [print_subscription(S, Now) || S <- Ss],
    'ok'.

print_subscription(JObj, Now) ->
    ExpiresIn = wh_json:get_integer_value(<<"expires">>, JObj) -
        (Now - wh_json:get_integer_value(<<"timestamp">>, JObj)),

    io:format(?SUBSCRIPTION_FORMAT_STR
              ,[[wh_json:get_value(<<"username">>, JObj), "@", wh_json:get_value(<<"realm">>, JObj)]
                ,wh_json:get_value(<<"from">>, JObj)
                ,wh_util:to_binary(ExpiresIn)
                ,wh_json:get_value(<<"event">>, JObj)
               ]).

-spec subscribe(ne_binary(), ne_binary()) -> 'ok'.
subscribe(Realm, User) ->
    Prop = [{<<"User">>, <<"sip:", User/binary, "@", Realm/binary>>}
            ,{<<"Expires">>, 1}
            ,{<<"Queue">>, <<>>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    case whapps_util:amqp_pool_request_custom(Prop
                                              ,fun wapi_presence:publish_subscribe/1
                                              ,fun wapi_presence:update_v/1
                                              ,{'presence', [{'restrict_to', ['updates']}]}
                                             )
    of
        {'ok', UpJObj} ->
            io:format("Sent subscription for ~s@~s, recv'd update of state ~s~n"
                      ,[User, Realm, wh_json:get_value(<<"State">>, UpJObj)]
                     );
        {'error', 'timeout'} ->
            io:format("Timed out waiting for update on subscription for ~s@~s~n", [User, Realm]);
        _E ->
            io:format("Sent subscription for ~s@~s, recv'd error: ~p~n", [User, Realm, _E])
    end.

-spec send_mwi_update(ne_binary(), ne_binary() | integer(), ne_binary() | integer() ) -> 'ok'.
send_mwi_update(User, New, Waiting) when is_binary(New) ->
  send_mwi_update(User, wh_util:to_integer(New), Waiting);
send_mwi_update(User, New, Waiting) when is_binary(Waiting) ->
  send_mwi_update(User, New, wh_util:to_integer(Waiting));
send_mwi_update(User, New, Waiting) ->
    Command = [{<<"Messages-New">>, New}
               ,{<<"Messages-Waiting">>, Waiting}
               ,{<<"Call-ID">>, wh_util:rand_hex_binary(16) }
               ,{<<"To">>, User}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    whapps_util:amqp_pool_send(Command, fun wapi_presence:publish_mwi_update/1),
    'ok'.

-spec list_terminated_callids() -> 'ok'.
list_terminated_callids() ->
    io:format("Here are the call IDs currently cached as terminated:~n", []),
    io:format("~s~n", [wh_util:join_binary(
                         omnip_subscriptions:cached_terminated_callids()
                         ,<<", ">>
                        )
                      ]).
