%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Maintenance functions for all
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnipresence_maintenance).

-include("omnipresence.hrl").

-export([current_subscriptions/0, current_subscriptions/1, current_subscriptions/2
         ,subscribe/2
        , send_mwi_update/3
        ]).

current_subscriptions() ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        ets:tab2list(omnip_subscriptions:table_id())
       )).

current_subscriptions(Realm) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions(wh_util:to_binary(Realm))
     )).

current_subscriptions(Realm, User) ->
    print_subscriptions(
      omnip_subscriptions:subscriptions_to_json(
        omnip_subscriptions:search_for_subscriptions(
          wh_util:to_binary(Realm), wh_util:to_binary(User)
         ))).

print_subscriptions([]) -> io:format("No subscriptions have been found~n");
print_subscriptions(Ss) ->
    Now = wh_util:current_tstamp(),
    io:format(" ~50.s | ~30s | ~10.s  |~n", [<<"Username@Realm">>, <<"From">>, <<"Expires">>]),
    [print_subscription(S, Now) || S <- Ss],
    'ok'.

print_subscription(JObj, Now) ->
    ExpiresIn = wh_json:get_integer_value(<<"expires">>, JObj) -
        (Now - wh_json:get_integer_value(<<"timestamp">>, JObj)),

    io:format(" ~50.s | ~30s | ~10.ss | ~20s |~n"
              ,[[wh_json:get_value(<<"username">>, JObj), "@", wh_json:get_value(<<"realm">>, JObj)]
                ,wh_json:get_value(<<"from">>, JObj)
                ,wh_util:to_binary(ExpiresIn)
                ,wh_json:get_value(<<"event">>, JObj)
               ]).

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
send_mwi_update(User, New, Saved ) when is_binary(New) ->
  send_mwi_update(User, wh_util:to_integer(New), Saved );
send_mwi_update(User, New, Saved ) when is_binary(Saved) ->
  send_mwi_update(User, New, wh_util:to_integer(Saved) );
send_mwi_update(User, New, Saved ) ->
    DefaultAccount = <<"sip:", User/binary>>,
    [Username, Realm] = binary:split(User, <<"@">>),
    Command = [{<<"Messages-New">>, New}
               ,{<<"Messages-Saved">>, Saved}
               ,{<<"Call-ID">>, wh_util:rand_hex_binary(16) }
               ,{<<"Notify-User">>, Username}
               ,{<<"Notify-Realm">>, Realm}
               ,{<<"Message-Account">>, DefaultAccount}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    whapps_util:amqp_pool_send(Command, fun wapi_notifications:publish_mwi_update/1),
    'ok'.

