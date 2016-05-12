%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% User document: tests
%%% @end
%%% @contributors
%%%   SIPLABS, LLS (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(kzd_user_test).

-include_lib("eunit/include/eunit.hrl").

-define(DOC, kz_json:set_values([{<<"first_name">>, "F"}
                                 ,{<<"last_name">>, "L"}
                                 ,{<<"email">>, <<"email@example.com">>}
                                 ,{<<"vm_to_email_enabled">>, 'true'}]
                                , kz_json:new())).
-define(EMPTY, kz_json:new()).

email_test_() ->
    [?_assertEqual(<<"email@example.com">>,   kzd_user:email(?DOC))
    ,?_assertEqual('undefined',               kzd_user:email(?EMPTY))
    ,?_assertEqual(<<"email@example.com">>,   kzd_user:email(?EMPTY, <<"email@example.com">>))
    ].

voicemail_notification_enabled_test_() ->
    [?_assertEqual('true',    kzd_user:voicemail_notification_enabled(?DOC))
    ,?_assertEqual('false',   kzd_user:voicemail_notification_enabled(?EMPTY))
    ,?_assertEqual('true',    kzd_user:voicemail_notification_enabled(?EMPTY, 'true'))
    ].

to_vcard_test() ->
    ?assertEqual(<<"BEGIN:VCARD\n"
                   "VERSION:3.0\n"
                   "FN:F L\n"
                   "N:L;F\n"
                   "EMAIL:email@example.com\n"
                   "END:VCARD">>
                 , kzd_user:to_vcard(?DOC)).
