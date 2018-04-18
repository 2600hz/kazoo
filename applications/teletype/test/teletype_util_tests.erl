%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-define(EMAILS_IN_TEMPLATE_1(Type), kz_json:from_list([{<<"to">>, kz_json:from_list([{<<"email_addresses">>, [<<"an_address@test.com">>]}, {<<"type">>, Type}])}])).
-define(EMAILS_IN_TEMPLATE_2(Type), kz_json:from_list([{<<"to">>, [<<"an_address@test.com">>]}])).
-define(EMAILS_IN_DATAJOBJ_1(Type), kz_json:from_list([{<<"to">>, kz_json:from_list([{<<"email_addresses">>, [<<"another_address@test.com">>]}, {<<"type">>, Type}])}])).
-define(EMAILS_IN_DATAJOBJ_2(Type), kz_json:from_list([{<<"to">>, [<<"another_address@test.com">>]}])).

find_addresses_test_() ->
    [{"find_addresses for type " ++ kz_term:to_list(Type)
     ,generate_find_addresses_test(Type)
     }
     || Type <- [?EMAIL_SPECIFIED, ?EMAIL_ORIGINAL]%%, 'undefined', ?EMAIL_ADMINS]
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
