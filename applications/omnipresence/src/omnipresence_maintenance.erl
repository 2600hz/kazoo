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
         ,current_presence_state/2
         ,subscribe/2
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

    io:format(" ~50.s | ~30s | ~10.ss |~n"
              ,[[wh_json:get_value(<<"username">>, JObj), "@", wh_json:get_value(<<"realm">>, JObj)]
                ,wh_json:get_value(<<"from">>, JObj)
                ,wh_util:to_binary(ExpiresIn)
               ]).

current_presence_state(Realm, User) ->
    print_presence_state(
      omnip_presences:find_presence_state(Realm, User)
     ).

print_presence_state({'error', 'not_found'}) ->
    io:format("no presence state found~n");
print_presence_state({'ok', PS}) ->
    io:format(" ~50.s | ~10.s | ~30.s |~n", [<<"Endpoint">>, <<"Presence">>, <<"Timestamp">>]),
    io:format(" ~50.s | ~10.s | ~30.s |~n"
              ,[omnip_presences:user(PS)
                ,omnip_presences:current_state(PS)
                ,wh_util:pretty_print_datetime(omnip_presences:timestamp(PS))
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
