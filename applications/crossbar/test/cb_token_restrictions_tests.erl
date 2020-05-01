%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_token_restrictions_tests).

-include_lib("eunit/include/eunit.hrl").
-include("cb_token_restrictions_test.hrl").

-define(DENY_REQ, 'true').
-define(ALLOW_REQ, 'false').

allow_all_rule_test_() ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2, auth_doc(?ALLOW_ALL_RULE_RESTRICTIONS)}
                           ]
                          ),
    Label = "Verify catch-all authorizes request",

    build_assertions(Label, Context, [?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ]).

deny_api_endpoint_test_() ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2, auth_doc(?DENY_API_ENDPOINT_RESTRICTIONS)}
                           ]
                          ),
    Label = "Verify denied access to nonexistent endpoint in restrictions",

    build_assertions(Label, Context, [?DENY_REQ, ?DENY_REQ, ?DENY_REQ, ?DENY_REQ]).

allow_api_endpoint_test_() ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2, auth_doc(?ALLOW_API_ENDPOINT_RESTRICTIONS)}
                           ]
                          ),
    Label = "Verify allowed access to api endpoint in restrictions",

    build_assertions(Label, Context, [?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ]).

allow_accounts_test_() ->
    Accounts = [{?ACCOUNT_AUTH, ?DENY_REQ} %% auth can only change auth
               ,{?ACCOUNT_DESCENDANT, ?ALLOW_REQ} %% auth can change descendant
               ,{?ACCOUNT_ID, ?ALLOW_REQ} %% explicit account
               ,{?AUTH_ACCOUNT_ID, ?DENY_REQ} %% explicit auth account
               ,{'undefined', ?ALLOW_REQ} %% missing "allowed_accounts"
               ],
    [allow_accounts_assertions(Account) || Account <- Accounts].

allow_accounts_assertions({Account, Expected}) ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2
                            ,auth_doc(?ALLOW_ACCOUNTS_RESTRICTIONS(Account))
                            }
                           ]
                          ),
    Label = "Verify allowed access to accounts in endpoint restrictions",

    build_assertions(Label, Context, [Expected, Expected, Expected, Expected]).

argument_test_() ->
    ArgPatterns =
        [{?ARGS_EMPTY, [?ALLOW_REQ, ?DENY_REQ, ?DENY_REQ, ?DENY_REQ]}
        ,{?ARGS_SINGLE, [?DENY_REQ, ?ALLOW_REQ, ?DENY_REQ, ?DENY_REQ]}
        ,{?ARGS_ANY, [?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ]}
        ,{?ARGS_EXACT, [?DENY_REQ, ?ALLOW_REQ, ?DENY_REQ, ?DENY_REQ]}
        ,{?ARGS_ANY_TWO, [?DENY_REQ, ?DENY_REQ, ?ALLOW_REQ, ?DENY_REQ]}
        ,{?ARGS_ANY_THREE, [?DENY_REQ, ?DENY_REQ, ?DENY_REQ, ?ALLOW_REQ]}
        ,{?ARGS_EXACT_THEN_ANY, [?DENY_REQ, ?ALLOW_REQ, ?ALLOW_REQ, ?ALLOW_REQ]}
        ],
    [argument_assertions(ArgPattern) || ArgPattern <- ArgPatterns].

argument_assertions({ArgPattern, ExpectedResults}) ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2
                            ,auth_doc(?ARGUMENTS_RESTRICTIONS(ArgPattern))
                            }
                           ]
                          ),

    Label = "Verify argument pattern matching works",
    build_assertions(Label, Context, ExpectedResults).

http_method_test_() ->
    Methods = [{?HTTP_ANY, ?ALLOW_REQ}
              ,{?HTTP_GET_ONLY, ?ALLOW_REQ}
              ,{?HTTP_GET_POST, ?ALLOW_REQ}
              ,{?HTTP_POST_ONLY, ?DENY_REQ}
              ],
    [method_assertions(Method) || Method <- Methods].

method_assertions({Verbs, Expected}) ->
    Context =
        cb_context:setters(cb_context:new()
                          ,[{fun cb_context:set_auth_account_id/2, ?AUTH_ACCOUNT_ID}
                           ,{fun cb_context:set_account_id/2, ?ACCOUNT_ID}
                           ,{fun cb_context:set_req_verb/2, ?HTTP_GET}
                           ,{fun cb_context:set_auth_doc/2
                            ,auth_doc(?HTTP_VERB_RESTRICTIONS(Verbs))
                            }
                           ]
                          ),

    Label = "Verify HTTP Verb matching works",
    build_assertions(Label, Context, [Expected, Expected, Expected, Expected]).

-define(TEST_ARGS, [[]
                   ,[?DEVICE_ID]
                   ,[?DEVICE_ID, <<"sync">>]
                   ,[?DEVICE_ID, <<"quick_call">>, <<"+14158867900">>]
                   ]).

build_assertions(Label, Context, ExpectedResults) ->
    [{Label
     ,?_assertEqual(Result, maybe_deny_access(Context, ReqParams))
     }
     || {ReqParams, Result} <- lists:zip(?TEST_ARGS, ExpectedResults)
    ].


maybe_deny_access(Context, ReqParams) ->
    cb_token_restrictions:maybe_deny_access(
      cb_context:set_req_nouns(Context
                              ,[{<<"devices">>, ReqParams}
                               ,{<<"accounts">>, ?ACCOUNT_ID}
                               ]
                              )
     ).

auth_doc(Restrictions) ->
    kz_json:from_list([{<<"restrictions">>, Restrictions}]).
