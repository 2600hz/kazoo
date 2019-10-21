%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_port_requests).

%% Manual testing
-export([seq/0
        ,cleanup/0

        ,test/0
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_proper.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(ACCOUNTS_SETTINGS
       ,#{<<"pqc_ports_normal">> => #{<<"numbers">> => [<<"+13335551234">>]
                                     ,<<"parent_account">> => master
                                     ,<<"port_authority">> => master
                                     }
         ,<<"pqc_ports_authority">> => #{<<"numbers">> => [<<"+13335554567">>]
                                        ,<<"is_port_authority">> => true
                                        ,<<"parent_account">> => master
                                        ,<<"port_authority">> => master
                                        }
         ,<<"pqc_ports_sub01">> => #{<<"numbers">> => [<<"+13335554321">>]
                                    ,<<"parent_account">> => <<"pqc_ports_authority">>
                                    ,<<"port_authority">> => <<"pqc_ports_authority">>
                                    }
         }
       ).

%% sort child first
-define(ACCOUNT_NAMES
       ,[<<"pqc_ports_normal">>
        ,<<"pqc_ports_sub01">>
        ,<<"pqc_ports_authority">>
        ]
       ).

-define(NUMBERS, (maps:fold(fun(_, #{<<"numbers">> := Nums}, Acc) -> Acc ++ Nums end
                           ,[]
                           ,?ACCOUNTS_SETTINGS
                           )
                 )
       ).

-type state() :: map().

%%------------------------------------------------------------------------------
%% @doc Required for complinig and passing edocification.
%%
%% This function is required because EUnit is adding this function autmatically
%% if it is _not_ defined using parse transform.
%%
%% Also our Makefile is required spec for exported function and this automatically
%% added function by EUnit doesn't have spec, make this module not compilable.
%% @end
%%------------------------------------------------------------------------------
-spec test() -> any().
test() ->
    eunit:test(?MODULE).

-spec seq() -> any().
seq() ->
    _ = init_system(),
    'ok' = eunit:test([{'setup'
                       ,fun initial_state/0
                       ,fun cleanup/1
                       ,fun all_port_seq/1
                       }
                      ]
                     ,[verbose]
                     ).

-spec init_system() -> 'ok'.
init_system() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'teletype']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_port_requests'
                   ,'cb_whitelabel'
                   ,'cb_comments'
                   ]
        ],
    'ok'.

-spec initial_state() -> state().
initial_state() ->
    TestId = kz_binary:rand_hex(16),
    kz_log:put_callid(TestId),

    API = pqc_cb_api:authenticate(),
    State = #{master => #{model => pqc_kazoo_model:new(API)
                         ,account_id => pqc_cb_api:auth_account_id(API)
                         ,account_name => kzd_accounts:fetch_name(pqc_cb_api:auth_account_id(API))
                         }
             },

    try
        lists:foldl(fun(Name, Acc) -> initialize_account(API, Name, Acc) end
                   ,State
                   ,lists:reverse(?ACCOUNT_NAMES)
                   )
    catch
        ?STACKTRACE(_R, _T, ST)
        ?debugFmt("exception ~p:~p", [_R, _T]),
        kz_log:log_stacktrace(ST),
        cleanup(State),
        throw('failed')
        end.

-spec initialize_account(pqc_cb_api:state(), kz_term:ne_binary(), state()) -> state().
initialize_account(API, AccountName, StateAcc) ->
    Account = create_account(API, AccountName, StateAcc),
    'true' = kz_term:is_ne_binary(Account),

    AccountJObj = pqc_cb_response:data(Account),
    AccountId = kz_doc:id(AccountJObj),
    ?debugFmt("created account ~s(~s)", [AccountName, AccountId]),

    _ = timer:sleep(1000), %% too fast for kazoo

    UserResp = create_admin_user(API, AccountId, AccountName),
    'true' = kz_term:is_ne_binary(UserResp),
    User = pqc_cb_response:data(UserResp),

    AccountApi = pqc_cb_api:authenticate(kzd_accounts:name(AccountJObj)
                                        ,kzd_users:username(User)
                                        ,<<"qwerty">>
                                        ),

    _ = timer:sleep(1000), %% too fast for kazoo

    IsAuthority = maps:get(<<"is_port_authority">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS), 'false'),
    Whitelabel = create_whitelabel(AccountApi, AccountId, AccountName, IsAuthority),
    'true' = kz_term:is_ne_binary(Whitelabel),

    StateAcc#{AccountName => #{model => pqc_kazoo_model:new(AccountApi)
                              ,account_id => AccountId
                              ,account_name => AccountName
                              ,admin => User
                              }
             }.

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary(), state()) -> pqc_cb_api:response().
create_account(API, AccountName, State) ->
    ParentName = maps:get(<<"parent_account">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS)),
    AccountId = maps:get(account_id, maps:get(ParentName, State)),
    pqc_cb_accounts:create_account(API, AccountName, AccountId).

-spec create_admin_user(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_admin_user(API, AccountId, AccountName) ->
    Email = <<"admin@", AccountName/binary, ".com">>,
    Setters = [{fun kzd_users:set_first_name/2, <<AccountName/binary, "-admin">>}
              ,{fun kzd_users:set_last_name/2, <<"LastName">>}
              ,{fun kzd_users:set_email/2, Email}
              ,{fun kzd_users:set_username/2, Email}
              ,{fun kzd_users:set_priv_level/2, <<"admin">>}
              ,{fun kzd_users:set_password/2, <<"qwerty">>}
              ],
    Envelope =
        pqc_cb_api:create_envelope(
          kz_json:set_value(<<"send_email_on_creation">>
                           ,false
                           ,kz_doc:setters(kzd_users:new(), Setters)
                           )
         ),

    Url = string:join([pqc_cb_accounts:account_url(AccountId), "users"], "/"),
    Expectations = [#expectation{response_codes = [201]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,Url
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec create_whitelabel(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), boolean()) ->
                               pqc_cb_api:response().
create_whitelabel(_, _, _, 'false') ->
    <<"no whitelable for you">>;
create_whitelabel(API, AccountId, AccountName, 'true') ->
    Setters = [{fun kzd_whitelabel:set_port_authority/2, AccountId}
              ,{fun kzd_whitelabel:set_port_support_email/2, <<"port_agent@", AccountName/binary, ".com">>}
              ],
    pqc_cb_whitelabel:create_whitelabel(API, Setters).

-spec cleanup() -> 'ok'.
cleanup() ->
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(state()) -> 'ok'.
cleanup(#{master := #{model := Model}}) ->
    cleanup(pqc_kazoo_model:api(Model));
cleanup(API) ->
    ?debugFmt("do cleanup, okay?", []),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = cleanup_port_requests_db(),
    _ = pqc_cb_api:cleanup(API),
    'ok'.

-spec cleanup_port_requests_db() -> 'ok'.
cleanup_port_requests_db() ->
    ViewOptions = [{'keys', ?NUMBERS}],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, <<"port_requests/listing_by_number">>, ViewOptions) of
        {'ok', []} -> ok;
        {'ok', Ports} ->
            Ids = [kz_doc:id(P) || P <- Ports],
            ?debugFmt("deleting ~p", [Ids]),
            {ok, Result} = kz_datamgr:del_docs(?KZ_PORT_REQUESTS_DB, Ids),
            _ = kz_datamgr:flush_cache_docs(?KZ_PORT_REQUESTS_DB),
            _ = [?debugFmt("unabled to delete ~s: ~p", [kz_doc:id(Res), kz_json:get_value(<<"error">>, Res)])
                 || Res <- Result,
                    kz_term:is_ne_binary(kz_json:get_ne_binary_value(<<"error">>, Res))
                ],
            'ok';
        {'error', _R} ->
            ?debugFmt("uanbled to delete port requests: ~p", [_R])
    end.

-spec all_port_seq(state()) -> term().
all_port_seq(State) ->
    [{"creating ports", create_ports_seq(State)}
    ,{"test agent port api", port_agent_seq(State)}
    ,{"test account port api", port_account_seq(State)}
    ,{"test descendants port api", port_descendants_seq(State)}
    ].

-spec create_ports_seq(state()) -> term().
create_ports_seq(State) ->
    [create_ports_seq(State, AccountName)
     || AccountName <- ?ACCOUNT_NAMES
    ].

-spec create_ports_seq(state(), kz_term:ne_binary()) -> term().
create_ports_seq(State, AccountName) ->
    Model = maps:get('model', maps:get(AccountName, State)),
    API = pqc_kazoo_model:api(Model),

    Created = create_port(API, AccountName),

    [{"creating ports for '" ++ kz_term:to_list(AccountName) ++ "'"
     ,?IF(kz_term:is_ne_binary(Created), create_ports_seq(State, AccountName, Created), ?_assert(false))
     }
    ].

-spec create_ports_seq(state(), kz_term:ne_binary(), pqc_cb_api:response()) -> term().
create_ports_seq(State, AccountName, Resp) ->
    WhoIsPortAuthority = maps:get(<<"port_authority">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS)),
    AuthorityId = maps:get('account_id', maps:get(WhoIsPortAuthority, State)),
    AuthorityName = maps:get('account_name', maps:get(WhoIsPortAuthority, State)),

    JObj = pqc_cb_response:data(Resp),
    ReadOnly = kz_json:get_json_value(<<"_read_only">>, JObj, kz_json:new()),
    PortId = kz_doc:id(JObj),

    [{"port id '" ++ kz_term:to_list(PortId) ++ "'"
     ,?_assert(kz_term:is_ne_binary(PortId))
     }
    ,{"checking state"
     ,?_assertEqual(?PORT_UNCONFIRMED, kz_json:get_ne_binary_value(<<"port_state">>, JObj))
     }
    ,{"account name"
     ,?_assertEqual(AccountName, kz_json:get_ne_binary_value(<<"account_name">>, ReadOnly))
     }
    ,{"authority id"
     ,?_assertEqual(AuthorityId, kz_json:get_ne_binary_value(<<"port_authority">>, ReadOnly))
     }
    ,{"authority name '" ++ kz_term:to_list(AuthorityName) ++ "'"
     ,?_assertEqual(AuthorityName, kz_json:get_ne_binary_value(<<"port_authority_name">>, ReadOnly))
     }
    ].

-spec port_agent_seq(state()) -> term().
port_agent_seq(State) ->
    [{"listing test", port_agent_list_seq(State)}].

-spec port_agent_list_seq(state()) -> term().
port_agent_list_seq(State) ->
    self_list_seq(State, <<"pqc_ports_authority">>).

-spec self_list_seq(state(), kz_term:ne_binary()) -> term().
self_list_seq(State, AccountName) ->
    [{'setup'
     ,fun() ->
              #{'model' := Model} = maps:get(AccountName, State),
              list_account_ports(pqc_kazoo_model:api(Model))
      end
     ,fun(Fetched) ->
              [{"non-empty result", ?_assert(kz_term:is_ne_binary(Fetched))}
              ,?IF(kz_term:is_ne_binary(Fetched)
                  ,self_list_seq(State, AccountName, Fetched)
                  ,[]
                  )
              ]
      end
     }
    ].

-spec self_list_seq(state(), kz_term:ne_binary(), pqc_cb_api:response()) -> term().
self_list_seq(State, AccountName, Resp) ->
    WhoIsPortAuthority = maps:get(<<"port_authority">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS)),
    AuthorityId = maps:get('account_id', maps:get(WhoIsPortAuthority, State)),
    AuthorityName = maps:get('account_name', maps:get(WhoIsPortAuthority, State)),

    PortNumbers = maps:get(<<"numbers">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS)),

    JObj = pqc_cb_response:data(Resp),
    AccountList = [Acc || Acc <- JObj,
                          kz_json:get_value(<<"account_name">>, Acc) =:= AccountName
                  ],
    [[{"account '"++ kz_term:to_list(AccountName) ++ "' has ports"
      ,?_assert(kz_term:is_not_empty(kz_json:get_list_value(<<"port_requests">>, hd(AccountList), [])))
      }
      | [{"has port for '" ++ kz_term:to_list(Num) ++ "'"
         ,?_assert(kz_term:is_not_empty([N || A <- AccountList,
                                              Ps <- kz_json:get_value(<<"port_requests">>, A),
                                              N <- kz_json:get_keys(kzd_port_requests:numbers(Ps, kz_json:new())),
                                              N =:= Num
                                        ])
                  )
         }
         || Num <- PortNumbers
        ]
     ]
     | [[{"authority id '" ++ kz_term:to_list(AuthorityId) ++ "'"
         ,?_assertEqual(AuthorityId, kz_json:get_value([<<"_read_only">>, <<"port_authority">>], Port))
         }
        ,{"authority name '" ++ kz_term:to_list(AuthorityName) ++ "'"
         ,?_assertEqual(AuthorityName, kz_json:get_value([<<"_read_only">>, <<"port_authority_name">>], Port))
         }
        ]
        || AccountPorts <- JObj,
           Port <- kz_json:get_list_value(<<"port_requests">>, AccountPorts, [])
       ]
    ].

-spec port_account_seq(state()) -> term().
port_account_seq(#{master := #{model := Model}}) ->
    API = pqc_kazoo_model:api(Model),
    [{"get master's account ports"
     ,?_assertEqual('true', have_ports(list_account_ports(API)))
     }
    ].

-spec port_descendants_seq(state()) -> term().
port_descendants_seq(#{master := #{model := Model}}) ->
    API = pqc_kazoo_model:api(Model),
    [{"get all master account descendants's ports"
     ,?_assertEqual('true', have_ports(list_descendants_ports(API)))
     }
    ].

-spec have_ports(pqc_cb_api:response()) -> boolean().
have_ports({'error', Error}) ->
    ?debugFmt("we have some rror:~n~p~n", [Error]),
    'false';
have_ports(Resp) ->
    JObj = kz_json:decode(Resp),
    kz_json:get_ne_binary_value(<<"status">>, JObj, <<"error">>) =/= <<"error">>
        andalso is_list(kz_json:get_list_value(<<"data">>, JObj)).

%% -spec ports_agent_url() -> string().
%% ports_agent_url() ->
%%     string:join([pqc_cb_api:v2_base_url(), "port_requests"], "/").

-spec ports_account_url(string() | kz_term:ne_binary()) -> string().
ports_account_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "port_requests"], "/").

-spec ports_descendants_url(string() | kz_term:ne_binary()) -> string().
ports_descendants_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "descendants", "port_requests"], "/").

-spec create_port(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_port(API, AccountName) ->
    AccountId = pqc_cb_api:auth_account_id(API),
    Envelope = pqc_cb_api:create_envelope(seed_ports(AccountName)),

    Expectations = [#expectation{response_codes = [201]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,ports_account_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

%% -spec list_agent_ports(pqc_cb_api:state()) -> pqc_cb_api:response().
%% list_agent_ports(API) ->
%%     pqc_cb_api:make_request([#{'response_codes' => [200]}]
%%                            ,fun kz_http:get/2
%%                            ,ports_agent_url()
%%                            ,pqc_cb_api:request_headers(API)
%%                            ).

-spec list_account_ports(pqc_cb_api:state()) -> pqc_cb_api:response().
list_account_ports(API) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,ports_account_url(pqc_cb_api:auth_account_id(API))
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec list_descendants_ports(pqc_cb_api:state()) -> pqc_cb_api:response().
list_descendants_ports(API) ->
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,ports_descendants_url(pqc_cb_api:auth_account_id(API))
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec seed_ports(kz_term:ne_binary()) -> kz_json:object().
seed_ports(AccountName) ->
    Rand = kz_binary:rand_hex(1),
    PortName = <<AccountName/binary, "-", Rand/binary>>,
    SendTo = <<PortName/binary, "@", AccountName/binary, ".com">>,

    Numbers = kz_json:from_list([{Num, kz_json:new()}
                                 || Num <- maps:get(<<"numbers">>, maps:get(AccountName, ?ACCOUNTS_SETTINGS))
                                ]),

    Setters = [{fun kzd_port_requests:set_bill_account_number/2, <<"Seed">>}
              ,{fun kzd_port_requests:set_bill_btn/2, <<"+15559993232">>}
              ,{fun kzd_port_requests:set_bill_carrier/2, <<"Hello">>}
              ,{fun kzd_port_requests:set_bill_locality/2, <<"CA">>}
              ,{fun kzd_port_requests:set_bill_name/2, <<"Seed">>}
              ,{fun kzd_port_requests:set_bill_pin/2, <<"1234">>}
              ,{fun kzd_port_requests:set_bill_postal_code/2, <<"11111">>}
              ,{fun kzd_port_requests:set_bill_street_address/2, <<"Somewhere">>}
              ,{fun kzd_port_requests:set_bill_street_number/2, <<"1">>}
              ,{fun kzd_port_requests:set_bill_street_type/2, <<"Freeway">>}
              ,{fun kzd_port_requests:set_name/2, PortName}
              ,{fun kzd_port_requests:set_notifications_email_send_to/2, SendTo}
              ,{fun kzd_port_requests:set_numbers/2, Numbers}
              ,{fun kzd_port_requests:set_signee_name/2, <<"kazoo_proper">>}
              ,{fun kzd_port_requests:set_signing_date/2, kz_time:now_s()}
              ,{fun kzd_port_requests:set_transfer_date/2, kz_time:now_s() + (10 * ?SECONDS_IN_DAY)}
              ],
    kz_doc:setters(kz_json:new(), Setters).
