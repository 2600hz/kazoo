%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-define(TO_EMAILS(Emails), kz_json:from_list([{<<"to">>, Emails}])).
-define(TO_EMAILS(Emails, Type), kz_json:from_list([{<<"to">>, kz_json:from_list([{<<"email_addresses">>, Emails}, {<<"type">>, Type}])}])).

-define(EMAILS_IN_TEMPLATE_1(Type), ?TO_EMAILS([<<"an_address@test.com">>], Type)).
-define(EMAILS_IN_TEMPLATE_2(Type), ?TO_EMAILS([<<"an_address@test.com">>])).
-define(EMAILS_IN_DATAJOBJ_1(Type), ?TO_EMAILS([<<"another_address@test.com">>], Type)).
-define(EMAILS_IN_DATAJOBJ_2(Type), ?TO_EMAILS([<<"another_address@test.com">>])).

find_addresses_test_() ->
    [{"find_addresses for type " ++ kz_term:to_list(Type)
     ,generate_find_addresses_test(Type)
     }
     || Type <- [?EMAIL_SPECIFIED, ?EMAIL_ORIGINAL]%%, 'undefined', ?EMAIL_ADMINS]
    ] ++ [{"check email address value", check_address_value()}
         ].

generate_find_addresses_test(?EMAIL_SPECIFIED=Type) ->
    [{"check defined in template with path 'to.email_addresses' even if emails are defined in datajobj"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_1(Type), ?EMAILS_IN_TEMPLATE_1(Type), <<"some_notification">>))
                   )
     }
    ,{"fallback to defined in template with path 'to' even if emails are defined in datajobj"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_1(Type), ?EMAILS_IN_TEMPLATE_2(Type), <<"some_notification">>))
                   )
     }
    ,{"fallback to datajobj with path 'to.email_addresses' if not found in template"
     ,?_assertEqual([<<"another_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_1(Type), kz_json:new(), <<"some_notification">>))
                   )
     }
    ,{"fallback to datajobj with path 'to' if not found in template"
     ,?_assertEqual([<<"another_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_2(Type), kz_json:from_list([{<<"type">>, Type}]), <<"some_notification">>))
                   )
     }
    ];
generate_find_addresses_test(?EMAIL_ORIGINAL=Type) ->
    [{"check defined in datajobj with path 'to' even if emails are defined in template"
     ,?_assertEqual([<<"another_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_2(Type), ?EMAILS_IN_TEMPLATE_1(Type), <<"some_notification">>))
                   )
     }
    ,{"fallback to defined in datajobj with path 'to.email_addresses' even if emails are defined in templates"
     ,?_assertEqual([<<"another_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?EMAILS_IN_DATAJOBJ_1(Type), ?EMAILS_IN_TEMPLATE_1(Type), <<"some_notification">>))
                   )
     }
    ,{"fallback to template with path 'to' if not found in datajobj"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(kz_json:from_list([{<<"type">>, Type}]), ?EMAILS_IN_TEMPLATE_2(Type), <<"some_notification">>))
                   )
     }
    ,{"fallback to template with path 'to.email_addresses' if not found in datajobj"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(kz_json:from_list([{<<"type">>, Type}]), ?EMAILS_IN_TEMPLATE_1(Type), <<"some_notification">>))
                   )
     }
    ].

check_address_value() ->
    [{"email is undefined"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS('undefined'), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is an empty binary"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS(<<>>), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a binary"
     ,?_assertEqual([<<"a_binary@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS(<<"a_binary@test.com">>), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is an empty list"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS([]), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a list binaries (email addresses with domain)"
     ,?_assertEqual([<<"a@test.com">>, <<"b@test.com">>, <<"c@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS([<<"a@test.com">>, <<"b@test.com">>, <<"c@test.com">>]), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a list non empty list with some non email addresesses"
     ,?_assertEqual([<<"a@test.com">>, <<"b@test.com">>]
                   ,props:get_value(<<"to">>
                                   ,teletype_util:find_addresses(?TO_EMAILS([<<"a@test.com">>
                                                                            ,<<"btest.com">>
                                                                            ,"yohoo@yahoo.com"
                                                                            ,""
                                                                            ,'undefined'
                                                                            ,12
                                                                            ,<<>>
                                                                            ,[]
                                                                            ,1.2
                                                                            ,<<"this_is_a_real_email_address_believe_me">>
                                                                            ,<<"no_i_am@n.em@il.address">>
                                                                            ,maps:new()
                                                                            ,kz_json:new()
                                                                            ,<<"b@test.com">>
                                                                            ]), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a JSON"
     ,?_assertEqual([<<"a_json@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS([<<"a_json@test.com">>], ?EMAIL_ORIGINAL), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a string"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS("a_string@test.com"), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ,{"email is a map"
     ,?_assertEqual([<<"an_address@test.com">>]
                   ,props:get_value(<<"to">>, teletype_util:find_addresses(?TO_EMAILS(maps:new()), ?EMAILS_IN_TEMPLATE_1(?EMAIL_ORIGINAL), <<"some_notification">>))
                   )
     }
    ].
