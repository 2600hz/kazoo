%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_callflow_tests).

-include_lib("eunit/include/eunit.hrl").

-include("kz_documents.hrl").

-define(FLOW, <<"flow">>).

simple_successful_validation_test_() ->
    ChildData = kz_json:from_list([{<<"key1">>, <<"value1">>}
                                  ,{<<"key2">>, 1}
                                  ]
                                 ),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ]
                             ),
    Doc = kz_json:from_list([{?FLOW, Child}]),

    {'ok', ValidatedDoc} = kzd_callflows:validate_flow(Doc),

    ValidatedFlow = kzd_callflows:flow(ValidatedDoc),

    [?_assertEqual(<<"value3">>, kz_json:get_value([<<"data">>, <<"key3">>], ValidatedFlow))].

nested_children_test_() ->
    ChildData = kz_json:from_list([{<<"key1">>, <<"value1">>}
                                  ,{<<"key2">>, 1}
                                  ,{<<"key3">>, <<"top">>}
                                  ]
                                 ),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ]
                             ),

    Nested = kz_json:set_value([<<"children">>, <<"_">>]
                              ,kz_json:delete_key([<<"data">>, <<"key3">>], Child)
                              ,Child
                              ),

    Doc = kz_json:from_list([{?FLOW, Nested}]),

    {'ok', ValidatedDoc} = kzd_callflows:validate_flow(Doc),
    ValidatedFlow = kzd_callflows:flow(ValidatedDoc),

    [?_assertEqual(<<"top">>, kz_json:get_value([<<"data">>, <<"key3">>], ValidatedFlow))
    ,?_assertEqual(<<"value3">>, kz_json:get_value([<<"children">>, <<"_">>, <<"data">>, <<"key3">>], ValidatedFlow))
    ].

simple_failed_test_() ->
    ChildData = kz_json:from_list([{<<"key1">>, 'true'}
                                  ,{<<"key2">>, 1}
                                  ]
                                 ),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ]
                             ),
    Doc = kz_json:from_list([{?FLOW, Child}]),

    {'error', Errors} = kzd_callflows:validate_flow(Doc),
    [?_assertMatch([{'data_invalid', _SchemaJObj, 'wrong_type', 'true', [<<"data">>, <<"key1">>]}], Errors)].

complex_failed_test_() ->
    ChildData = kz_json:from_list([{<<"key2">>, 'true'}]),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ]
                             ),
    Doc = kz_json:from_list([{?FLOW, Child}]),

    {'error', Errors} = kzd_callflows:validate_flow(Doc),
    [?_assertMatch([{'data_invalid', _Key2SchemaJObj, 'wrong_type', 'true', [<<"data">>, <<"key2">>]}
                   ,{'data_invalid', _Key1SchemaJObj, 'missing_required_property', _Value, [<<"data">>]}
                   ]
                  ,Errors
                  )
    ].

nested_children_failure_test_() ->
    ChildData = kz_json:from_list([{<<"key1">>, <<"value1">>}
                                  ,{<<"key2">>, 'true'}
                                  ,{<<"key3">>, <<"top">>}
                                  ]
                                 ),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ]
                             ),

    Nested = kz_json:set_value([<<"children">>, <<"_">>]
                              ,kz_json:delete_key([<<"data">>, <<"key3">>], Child)
                              ,Child
                              ),

    Doc = kz_json:from_list([{?FLOW, Nested}]),

    {'error', Errors} = kzd_callflows:validate_flow(Doc),

    [?_assertMatch([{'data_invalid', _SchemaJObj, 'wrong_type', 'true', [<<"children">>, <<"_">>, <<"data">>, <<"key2">>]}
                   ,{'data_invalid', _SchemaJObj, 'wrong_type', 'true', [<<"data">>, <<"key2">>]}
                   ]
                  ,Errors
                  )
    ].

action_failure_test_() ->
    Doc = kz_json:from_list([{?FLOW, kz_json:new()}]),
    {'error', Errors} = kzd_callflows:validate_flow(Doc),

    [?_assertEqual(1, length(Errors))
    ,?_assertMatch([{'data_invalid', _SchemaJObj
                    ,'missing_required_property'
                    ,<<"module">>
                    ,[]
                    }
                   ]
                  ,Errors
                  )
    ].

child_action_failure_test_() ->
    Children = kz_json:from_list([{<<"ca1">>, kz_json:new()}]),

    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, kz_json:new()}
                              ,{<<"children">>, Children}
                              ]),

    Doc = kz_json:from_list([{?FLOW, Child}]),
    {'error', Errors} = kzd_callflows:validate_flow(Doc),
    [?_assertEqual(3, length(Errors))
    ,?_assertMatch([{'data_invalid', _
                    ,'missing_required_property'
                    ,<<"module">>
                    ,[<<"children">>, <<"ca1">>]
                    }
                   ,{'data_invalid', _
                    ,'missing_required_property'
                    ,<<"key1">>
                    ,[<<"data">>]
                    }
                   ,{'data_invalid', _
                    ,'missing_required_property'
                    ,<<"key2">>
                    ,[<<"data">>]
                    }
                   ]
                  ,Errors
                  )
    ].

child_action_failure_data_test_() ->
    Children = kz_json:from_list([{<<"ca2">>, kz_json:from_list([{<<"module">>, <<"test">>}])}]),

    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, kz_json:new()}
                              ,{<<"children">>, Children}
                              ]),

    Doc = kz_json:from_list([{?FLOW, Child}]),
    {'error', Errors} = kzd_callflows:validate_flow(Doc),

    [?_assertEqual(4, length(Errors))
    ,?_assertMatch([_, _, {'data_invalid', _SchemaJObj
                          ,'missing_required_property'
                          ,<<"key1">>
                          ,[<<"data">>]
                          }
                   , _]
                  ,Errors
                  )
    ].

multiple_child_action_failures_test_() ->
    Children = kz_json:from_list([{<<"mp1">>, kz_json:new()}
                                 ,{<<"mp2">>, kz_json:new()}
                                 ]),

    ChildData = kz_json:from_list([{<<"key1">>, <<"value1">>}
                                  ,{<<"key2">>, 1}
                                  ]
                                 ),
    Child = kz_json:from_list([{<<"module">>, <<"test">>}
                              ,{<<"data">>, ChildData}
                              ,{<<"children">>, Children}
                              ]),

    Doc = kz_json:from_list([{?FLOW, Child}]),
    {'error', Errors} = kzd_callflows:validate_flow(Doc),

    [?_assertEqual(2, length(Errors))
    ,?_assertMatch([{'data_invalid', _SchemaJObj
                    ,'missing_required_property'
                    ,<<"module">>
                    ,[<<"children">>, <<"mp1">>]
                    }
                   ,{'data_invalid', _SchemaJObj
                    ,'missing_required_property'
                    ,<<"module">>
                    ,[<<"children">>, <<"mp2">>]
                    }
                   ]
                  ,Errors
                  )
    ].
