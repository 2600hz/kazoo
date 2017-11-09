-module(webhooks_channel_util_test).

-include_lib("eunit/include/eunit.hrl").
-include("webhooks.hrl").

-define(IS_LOOPBACK_EVT, kz_json:from_list([{<<"Channel-Is-Loopback">>, 'true'}])).
-define(IS_LOOPBACK_ALT_EVT, [kz_json:from_list([{<<"Channel-Loopback-Leg">>, <<"A">>}])
                             ,kz_json:from_list([{<<"Channel-Loopback-Bowout-Execute">>, 'true'}])
                             ,kz_json:from_list([{<<"Channel-Loopback-Other-Leg-ID">>, <<"boo">>}])
                             ]).
-define(IS_NOT_LOOPBACK_EVT, kz_json:from_list([{<<"Hangup-Cause">>, <<"PEBCAK">>}])).

-define(INC_LOOPBACK_HOOK, #webhook{include_loopback='true'}).
-define(FILTER_LOOPBACK_HOOK, #webhook{include_loopback='false'}).

include_loopback_test_() ->
    Tests = [{?IS_LOOPBACK_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_LOOPBACK_EVT, ?FILTER_LOOPBACK_HOOK, 'false'}

            ,{?IS_NOT_LOOPBACK_EVT, ?INC_LOOPBACK_HOOK, 'true'}
            ,{?IS_NOT_LOOPBACK_EVT, ?FILTER_LOOPBACK_HOOK, 'true'}

             | lists:foldl(fun(Evt, Acc) ->
                                   [{Evt, ?INC_LOOPBACK_HOOK, 'true'}
                                   ,{Evt, ?FILTER_LOOPBACK_HOOK, 'false'}
                                    | Acc
                                   ]
                           end
                          ,[]
                          ,?IS_LOOPBACK_ALT_EVT
                          )
            ],
    [{hd(kz_json:get_keys(JObj))
     ,?_assertEqual(Include
                   ,webhooks_channel_util:is_fireable_hook(JObj, Hook)
                   )
     }
     || {JObj, Hook, Include} <- Tests
    ].
