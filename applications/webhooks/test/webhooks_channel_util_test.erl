-module(webhooks_channel_util_test).

-include_lib("eunit/include/eunit.hrl").
-include("webhooks.hrl").

-define(IS_LOOPBACK_EVT, kz_json:from_list([{<<"Channel-Is-Loopback">>, 'true'}])).
-define(IS_LOOPBACK_CHANNEL_EVT, kz_json:from_list([{<<"Channel-Name">>, <<"loopback/123-a">>}])).
-define(IS_NOT_LOOPBACK_CHANNEL_EVT, kz_json:from_list([{<<"Channel-Name">>, <<"sofia/sipinterface_1/123@abc">>}])).
-define(IS_NOT_LOOPBACK_EVT, kz_json:from_list([{<<"Hangup-Cause">>, <<"PEBCAK">>}])).

-define(INC_LOOPBACK_HOOK, #webhook{include_loopback='true'}).
-define(FILTER_LOOPBACK_HOOK, #webhook{include_loopback='false'}).

include_loopback_test_() ->
    Tests = [{?IS_LOOPBACK_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_LOOPBACK_EVT, ?FILTER_LOOPBACK_HOOK, 'false'}
            ,{?IS_LOOPBACK_CHANNEL_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_LOOPBACK_CHANNEL_EVT, ?FILTER_LOOPBACK_HOOK, 'false'}

            ,{?IS_NOT_LOOPBACK_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_NOT_LOOPBACK_EVT, ?FILTER_LOOPBACK_HOOK, 'true'}
            ,{?IS_NOT_LOOPBACK_CHANNEL_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_NOT_LOOPBACK_CHANNEL_EVT, ?FILTER_LOOPBACK_HOOK, 'true'}
            ],
    [{test_name(JObj)
     ,?_assertEqual(Include
                   ,webhooks_channel_util:is_fireable_hook(JObj, Hook)
                   )
     }
     || {JObj, Hook, Include} <- Tests
    ].

test_name(JObj) ->
    {[V|_], [K|_]} = kz_json:get_values(JObj),
    iolist_to_binary([K, " = ", kz_term:to_list(V)]).
