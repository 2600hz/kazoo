%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc User document: tests
%%% @author SIPLABS, LLS (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_user_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOC, kz_json:set_values([{<<"first_name">>, "F"}
                                ,{<<"last_name">>, "L"}
                                ,{<<"email">>, <<"email@example.com">>}
                                ,{<<"vm_to_email_enabled">>, 'true'}]
                               ,kz_json:new()
                               )
       ).
-define(EMPTY, kz_json:new()).

email_test_() ->
    [?_assertEqual(<<"email@example.com">>,   kzd_users:email(?DOC))
    ,?_assertEqual('undefined',               kzd_users:email(?EMPTY))
    ,?_assertEqual(<<"email@example.com">>,   kzd_users:email(?EMPTY, <<"email@example.com">>))
    ].

voicemail_notification_enabled_test_() ->
    [?_assertEqual('true',    kzd_users:vm_to_email_enabled(?DOC))
    ,?_assertEqual('false',   kzd_users:vm_to_email_enabled(?EMPTY, 'false'))
    ,?_assertEqual('true',    kzd_users:vm_to_email_enabled(?EMPTY, 'true'))
    ].

to_vcard_test() ->
    ?assertEqual(<<"BEGIN:VCARD\n"
                   "VERSION:3.0\n"
                   "FN:F L\n"
                   "N:L;F\n"
                   "EMAIL:email@example.com\n"
                   "END:VCARD"
                 >>
                ,kzd_users:to_vcard(?DOC)
                ).
