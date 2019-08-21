%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_duo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_auth.hrl").

%% Duo Configs
-define(IKEY, <<"DIXXXXXXXXXXXXXXXXXX">>).
-define(SKEY, <<"deadbeefdeadbeefdeadbeefdeadbeefdeadbeef">>).
-define(AKEY, <<"useacustomerprovidedapplicationsecretkey">>).
-define(USER, <<"testuser">>).
-define(API_HOST, <<"http://imaginary_app.whatever.com">>).


%% Common Values for tests
-define(SIG_RESP(AuthPart, AppPart)
       ,<<(AuthPart)/binary, ":", (AppPart)/binary>>
       ). %% Duo Signature Responses

-define(TEST_SIGN_CLAIM
       ,[{<<"owner_id">>, ?USER}
        ]
       ).
-define(TEST_VERIFY_CLAIM(AuthPart, AppPart)
       ,[{<<"mfa_resp">>, ?SIG_RESP(AuthPart, AppPart)}
         | ?TEST_SIGN_CLAIM
        ]
       ).

-define(TEST_CONFIG_JOBJ
       ,kz_json:from_list(
          [{<<"integration_key">>, ?IKEY}
          ,{<<"secret_key">>, ?SKEY}
          ,{<<"application_secret_key">>, ?AKEY}
          ,{<<"api_hostname">>, ?API_HOST}
          ]
         )
       ).

%% values for a successful authenticate request resulted in a
%% request singature to pass to Duo
-define(SIG_REQ_EXPIRE_1234
       ,<<"TX|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTQ4Njc2OTgwOQ==|28af5ae63742cfc52f36002a146ee181326cd40d:APP|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTQ4Njc2OTgwOQ==|1f02e643de667f188f409a13b7770dce0a1be777">>
       ).
-define(VALID_401_SETTINGS, kz_json:from_list(
                              [{<<"duo_sig_request">>, ?SIG_REQ_EXPIRE_1234}
                              ,{<<"duo_api_hostname">>, ?API_HOST}
                              ]
                             )
       ).
-define(VALID_401_MFA_RESP
       ,{'error', 401, kz_json:from_list([{<<"provider_name">>, <<"duo">>}
                                         ,{<<"settings">>, ?VALID_401_SETTINGS}
                                         ]
                                        )
        }
       ).

%% helper
-define(WRONG_XKEY(Key), kz_json:set_value(Key, <<"invalid">>, ?TEST_CONFIG_JOBJ)).
-define(WRONG_XKEY(Key, Value), kz_json:set_value(Key, Value, ?TEST_CONFIG_JOBJ)).

%% values for verify signature tests
-define(WRONG_IKEY, <<"DIXXXXXXXXXXXXXXXXXY">>).
-define(WRONG_AKEY, <<"invalidinvalidinvalidinvalidinvalidinvalidinvalidinvalid">>).

-define(VALID_SIG_AUTH_RESP, <<"AUTH|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTYxNTcyNzI0Mw==|d20ad0d1e62d84b00a3e74ec201a5917e77b6aef">>).

-define(INVALID_AUTH_RESP, <<"AUTH|INVALID|SIG">>).
-define(EXPIRED_AUTH_RESP, <<"AUTH|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTMwMDE1Nzg3NA==|cb8f4d60ec7c261394cd5ee5a17e46ca7440d702">>).
-define(WRONG_PARAMS_AUTH_RESP, <<"AUTH|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTYxNTcyNzI0M3xpbnZhbGlkZXh0cmFkYXRh|6cdbec0fbfa0d3f335c76b0786a4a18eac6cdca7">>).
-define(WRONG_PARAMS_APP_RESP, <<"APP|dGVzdHVzZXJ8RElYWFhYWFhYWFhYWFhYWFhYWFh8MTYxNTcyNzI0M3xpbnZhbGlkZXh0cmFkYXRh|7c2065ea122d028b03ef0295a4b4c5521823b9b5">>).

%% EUnits tests:

equal_sign_1234_test_() ->
    ConfigJObj = kz_json:set_values([{<<"duo_expire">>, ?TEST_DUO_SIGN_EXPIRE}
                                    ,{<<"app_expire">>, ?TEST_DUO_SIGN_EXPIRE}
                                    ]
                                   ,?TEST_CONFIG_JOBJ
                                   ),
    [{"Verify sign request is equal to a known computed sig_request"
     ,?_assertEqual(?VALID_401_MFA_RESP
                   ,kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ConfigJObj)
                   )
     }
    ].

sign_request_test_() ->
    [{"Verifying sign request without user"
     ,?_assertEqual('true'
                   ,is_validation_error(kz_mfa_duo:authenticate([], ?TEST_CONFIG_JOBJ))
                   )
     }
    ,{"Verifying sign request with invalid ikey"
     ,?_assertEqual('true'
                   ,is_validation_error(kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ?WRONG_XKEY(<<"integration_key">>)))
                   )
     }
    ,{"Verifying sign request with invalid skey"
     ,?_assertEqual('true'
                   ,is_validation_error(kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ?WRONG_XKEY(<<"secret_key">>)))
                   )
     }
    ,{"Verifying sign request with invalid akey"
     ,?_assertEqual('true'
                   ,is_validation_error(kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ?WRONG_XKEY(<<"application_secret_key">>)))
                   )
     }
    ,{"Verifying sign request without api_hostname"
     ,?_assertEqual('true'
                   ,is_validation_error(kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, kz_json:delete_key(<<"api_hostname">>, ?TEST_CONFIG_JOBJ)))
                   )
     }
    ,{"Verifying sign request with extra config variable"
     ,?_assertEqual('true'
                   ,is_challenge_resp(kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, kz_json:set_value(<<"happy_var">>, <<"happy_val">>, ?TEST_CONFIG_JOBJ)))
                   )
     }
    ].

is_validation_error({'error', {'configuration', <<"invalid duo configuration", _/binary>>}}) -> 'true';
is_validation_error(_) -> 'false'.

is_challenge_resp({error, 401, J}) -> kz_json:is_json_object(J);
is_challenge_resp(_) -> 'false'.

verify_request_test_() ->
    {_, _, SignReq} = kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ?TEST_CONFIG_JOBJ),
    [_, ValidAppSig] = binary:split(kz_json:get_value([<<"settings">>, <<"duo_sig_request">>], SignReq), <<":">>, ['global']),

    %% {_, _,InvalidSigReq} = kz_mfa_duo:authenticate(?TEST_SIGN_CLAIM, ?WRONG_XKEY(<<"application_secret_key">>, ?WRONG_AKEY)),
    %% [_, InvalidAppSig] = binary:split(InvalidSigReq, <<":">>, ['global']),

    [{"Verifying verify response with correct value"
     ,?_assertEqual({'ok', 'authenticated'}
                   ,kz_mfa_duo:authenticate(?TEST_VERIFY_CLAIM(?VALID_SIG_AUTH_RESP, ValidAppSig)
                                           ,?TEST_CONFIG_JOBJ
                                           )
                   )
     }
    ].
