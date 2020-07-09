-module(teletype_voicemail_to_email_tests).

-include_lib("eunit/include/eunit.hrl").

build_voicemail_data_test_() ->
    Mod = 'teletype_voicemail_to_email',
    Test = fun Mod:build_voicemail_data/1,
    Prefix = <<"voicemail.">>,
    Deprecated = [<<"box">>, <<"name">>],
    MaybeIncluded = [<<"transcription">>, <<"file_name">>, <<"file_type">>, <<"file_size">>],
    Macros = kz_json:get_keys(Mod:macros()),
    JObj = kz_json:from_list_recursive([{<<"voicemail_box">>, kz_binary:rand_hex(4)}
                                       ,{<<"voicemail_id">>, kz_binary:rand_hex(4)}
                                       ,{<<"vmbox_doc">>, [{<<"name">>, kz_binary:rand_hex(4)}
                                                          ,{<<"mailbox">>, kz_binary:rand_hex(4)}
                                                          ]}
                                       ]),

    [{"Asserting *" ++ kz_term:to_list(<<Prefix/binary, Macro/binary>>)
      ++ "* macro is listed when " ++ kz_term:to_list(Mod) ++ ":macros/0 is called"
     ,?_assert(lists:member(<<Prefix/binary, Macro/binary>>, Macros))
     } || Macro <- (props:get_keys(Test(JObj)) ++ MaybeIncluded) -- Deprecated
    ].
