%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_voicemail_tests).

-include_lib("eunit/include/eunit.hrl").

datetime_test_() ->
    qdate:set_timezone("GMT"),
    UTCDatetime = {{2020,3,16},{21,50,51}},

    [?_assertEqual(<<"March 16 2020 at 14:50">>, cf_voicemail:to_vm_datetime(<<"F j Y \\a\\t H:i">>, <<"America/Los_Angeles">>, UTCDatetime))
    ,?_assertEqual(<<"16 March 2020 at 22:50">>, cf_voicemail:to_vm_datetime(<<"j F Y \\a\\t H:i">>, <<"Europe/Paris">>, UTCDatetime))
    ].
