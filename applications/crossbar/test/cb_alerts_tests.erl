%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_alerts_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl"). %% PORT_SUSPENDED
-include_lib("kazoo_stdlib/include/kz_types.hrl"). %% SECONDS_IN_DAY

%%%=======================================================================================
%%% Tests generator
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cb_alerts_test_() -> [{string(), boolean()}].
cb_alerts_test_() ->
    [{"Check port requests", check_port_requests()}
    ,{"Check no plans financials", check_no_plans_financials()}
    ,{"Check low balance", check_low_balance()}
    ,{"Check payment token", check_payment_token()}
    ].

%%%=======================================================================================
%%% Tests
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_port_requests() -> [{string(), boolean()}].
check_port_requests() ->
    Mod = 'knm_port_request',
    ToMeck = ['kz_services_reseller', Mod],
    _ = lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, ToMeck),
    meck:expect('kz_services_reseller', 'is_reseller', fun(_) -> 'false' end),

    Context = cb_context:set_resp_data(cb_context:new(), []),
    {'ok', ActivePorts} = AccountActivePorts = account_active_ports(),

    %% 1 submitted, 1 unconfirmed, and 1 rejected.
    meck:expect(Mod, 'account_active_ports', fun(_) -> AccountActivePorts end),
    Context1 = cb_alerts:check_port_requests(Context),
    [Alert1, Alert2] = cb_context:resp_data(Context1),

    %% All ports (3) have last comment with `action_required=true' within the last comment
    %% and also there is 1 rejected and 1 unconfirmed.
    PortsWithComments = [add_comments(Port) || Port <- ActivePorts],
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'ok', PortsWithComments} end),
    Context2 = cb_alerts:check_port_requests(Context),

    %% Only 1 port with `action_required=true' within the last comment
    Port = add_comments(example_port_request()),
    %% Same as Port but last comment doesn't have `action_required=true'
    Port1 = swap_comments(Port),
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'ok', [Port, Port1]} end),
    Context3 = cb_alerts:check_port_requests(Context),
    [Alert3] = cb_context:resp_data(Context3),

    %% Not active ports found.
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'error', 'not_found'} end),
    Context4 = cb_alerts:check_port_requests(Context),

    %% Ports with state /= (unconfirmed|rejected) and no comments.
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'ok', [example_port_request()]} end),
    Context5 = cb_alerts:check_port_requests(Context),

    MeckValidate = lists:all(fun(Mecked) -> meck:validate(Mecked) end, ToMeck),
    lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

    [{"Only return ports with `last_comment`.action_required=true or state=(rejected|unconfirmed)"
     ,?_assertEqual({'true', <<"port_suspended">>},
                    {is_port_suspended_state(state_from_port_alert(Alert1))
                    ,category_from_alert(Alert1)
                    })
     }
    ,{"Only return ports with `last_comment`.action_required=true or state=(rejected|unconfirmed)"
     ,?_assertEqual({'true', <<"port_suspended">>},
                    {is_port_suspended_state(state_from_port_alert(Alert2))
                    ,category_from_alert(Alert2)
                    })
     }
    ,{"Only return ports with `last_comment`.action_required=true or state=(rejected|unconfirmed)"
     ,?_assertEqual(5, length(cb_context:resp_data(Context2)))
     }
    ,{"Only return ports with `last_comment`.action_required=true or state=(rejected|unconfirmed)"
     ,?_assertEqual({'true', <<"port_action_required">>},
                    {kz_json:get_value([<<"message">>, <<"action_required">>], Alert3)
                    ,category_from_alert(Alert3)
                    })
     }
    ,{"When not active ports are found the context should not change"
     ,?_assertEqual(Context4, Context)
     }
    ,{"When not ports with action_required=true or state=(unconfirmed|rejected) found the context should not change"
     ,?_assertEqual(Context5, Context)
     }
    ,{"Validate mecked modules"
     ,?_assert(MeckValidate)
     }
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_no_plans_financials() -> [{string(), boolean()}].
check_no_plans_financials() ->
    ToMeck = ['kz_services', 'kz_services_plans', 'kapps_util'],
    [{'setup'
     ,fun() -> [meck:new(M, ['passthrough']) || M <- ToMeck] end
     ,fun(_) -> [meck:unload(Mecked) || Mecked <- ToMeck] end
     ,fun(_) ->
              Context = cb_context:set_resp_data(cb_context:new(), []),

              %% simulate account has no plans
              meck:expect('kapps_util', 'get_master_account_id', fun() -> kz_binary:rand_hex(16) end),
              meck:expect('kz_services', 'fetch', fun(_) -> kz_services:empty() end),
              meck:expect('kz_services', 'plans', fun(_) -> kz_services_plans:empty() end),

              ContextNoPlans = cb_alerts:maybe_check_financials(Context),

              [{"No service plans, no financial alerts"
               ,?_assertEqual(ContextNoPlans, Context)
               }
              ,{"Validate mecked modules"
               ,?_assert(lists:all(fun(Mecked) -> meck:validate(Mecked) end, ToMeck))
               }
              ]
      end
     }].

-spec check_low_balance() -> [{string(), boolean()}].
check_low_balance() ->
    Context = cb_context:set_resp_data(cb_context:new(), []),
    ThresholdUSD = 5.0,
    DefaultThreshold = 'undefined',
    ThresholdNotConfigured = 0,
    PostPayAmountUnits = 20, %% expressed in units
    PostPayAmountUSD = kz_currency:units_to_dollars(PostPayAmountUnits),

    Mod = 'kz_currency',
    ToMeck = [Mod, 'kzd_accounts', 'kz_services_limits'],
    lists:foreach(fun(M) -> meck:new(M, ['passthrough']) end, ToMeck),

    meck:expect(Mod, 'available_dollars', fun(_) -> {'error', 'reason'} end),
    %% Limits with postpay disabled.
    meck:expect('kz_services_limits', 'fetch', fun(_) -> limits() end),

    %% error trying to get account's current balance and threshold configured
    meck:expect('kzd_accounts', 'low_balance_threshold', fun(_, _) -> ThresholdUSD end),
    Context1 = cb_alerts:check_low_balance(Context),

    %% error trying to get account's current balance and threshold NOT configured
    meck:expect('kzd_accounts',
                'low_balance_threshold',
                fun(_, _) -> DefaultThreshold end),
    Context2 = cb_alerts:check_low_balance(Context),

    %% Threshold NOT configured, PostPay DISABLED, and current balance = 1.
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', 1.0} end),
    Context3 = cb_alerts:check_low_balance(Context),

    %% Threshold NOT configured, PostPay DISABLED, and current balance = 0.
    ZeroBalance = 0.0,
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ZeroBalance} end),
    Context4 = cb_alerts:check_low_balance(Context),
    [Alert1] = cb_context:resp_data(Context4),

    %% Threshold NOT configured, PostPay DISABLED, and current balance < 0.
    NegativeBalance = -1.0,
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', NegativeBalance} end),
    Context5 = cb_alerts:check_low_balance(Context),
    [Alert2] = cb_context:resp_data(Context5),

    %% Threshold NOT configured, PostPay ENABLED and current balance < PostPayAmountUSD.
    BelowPostPayAmountUSD = PostPayAmountUSD - 1.0,
    meck:expect('kz_services_limits',
                'fetch',
                fun(_) -> limits_enabled_postpay(PostPayAmountUnits) end),
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', PostPayAmountUSD - 1.0} end),
    Context6 = cb_alerts:check_low_balance(Context),
    [Alert3] = cb_context:resp_data(Context6),

    %% Threshold NOT configured, PostPay ENABLED and current balance = PostPayAmountUSD.
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', PostPayAmountUSD} end),
    Context7 = cb_alerts:check_low_balance(Context),
    [Alert4] = cb_context:resp_data(Context7),

    %% Threshold NOT configured, PostPay ENABLED
    %% and current balance > PostPayAmountUSD.
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', PostPayAmountUSD + 1.0} end),
    Context8 = cb_alerts:check_low_balance(Context),

    %% Configure threshold
    meck:expect('kzd_accounts', 'low_balance_threshold', fun(_, _) -> ThresholdUSD end),

    %% Threshold configured and current balance < threshold.
    BelowThresholdUSD = ThresholdUSD - 1.0,
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', BelowThresholdUSD} end),
    Context9 = cb_alerts:check_low_balance(Context),
    [Alert5] = cb_context:resp_data(Context9),

    %% Threshold configured and current balance = threshold.
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD} end),
    Context10 = cb_alerts:check_low_balance(Context),

    %% Threshold configured and current balance > threshold.
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD + 1.0} end),
    Context11 = cb_alerts:check_low_balance(Context),

    MeckValidate = lists:all(fun(Mecked) -> meck:validate(Mecked) end, ToMeck),
    lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

    [{"If getting account's current balance fails the context should not change"
     ,?_assertEqual(Context1, Context)
     }
    ,{"If getting account's current balance fails the context should not change"
     ,?_assertEqual(Context2, Context)
     }
    ,{"If threshold not configured, postpay disabled, and current balance > 0 the context should not change"
     ,?_assertEqual(Context3, Context)
     }
    ,{"If threshold not configured, postpay disabled, and current balance == 0 raise an alert"
     ,?_assertEqual({ZeroBalance, ThresholdNotConfigured, <<"low_balance">>},
                    {available_from_low_balance_alert(Alert1)
                    ,threshold_from_low_balance_alert(Alert1)
                    ,category_from_alert(Alert1)
                    })
     }
    ,{"If threshold not configured, postpay disabled, and current balance < 0 raise an alert"
     ,?_assertEqual({NegativeBalance, ThresholdNotConfigured, <<"low_balance">>},
                    {available_from_low_balance_alert(Alert2)
                    ,threshold_from_low_balance_alert(Alert2)
                    ,category_from_alert(Alert2)
                    })
     }
    ,{"If threshold not configured, postpay enabled, and current balance < PostPayAmountUSD raise an alert"
     ,?_assertEqual({BelowPostPayAmountUSD, PostPayAmountUSD, <<"low_balance">>},
                    {available_from_low_balance_alert(Alert3)
                    ,threshold_from_low_balance_alert(Alert3)
                    ,category_from_alert(Alert3)
                    })
     }
    ,{"If threshold not configured, postpay enabled, and current balance == PostPayAmountUSD raise an alert"
     ,?_assertEqual({PostPayAmountUSD, PostPayAmountUSD, <<"low_balance">>},
                    {available_from_low_balance_alert(Alert4)
                    ,threshold_from_low_balance_alert(Alert4)
                    ,category_from_alert(Alert4)
                    })
     }
    ,{"If threshold not configured, postpay enabled, and current balance > PostPayAmountUSD the context should not change"
     ,?_assertEqual(Context8, Context)
     }
    ,{"If threshold configured and current balance < threshold raise an alert"
     ,?_assertEqual({BelowThresholdUSD, ThresholdUSD, <<"low_balance">>},
                    {available_from_low_balance_alert(Alert5)
                    ,threshold_from_low_balance_alert(Alert5)
                    ,category_from_alert(Alert5)
                    })
     }
    ,{"If threshold configured and current balance = threshold the context should not change"
     ,?_assertEqual(Context10, Context)
     }
    ,{"If threshold configured and current balance > threshold the context should not change"
     ,?_assertEqual(Context11, Context)
     }
    ,{"Validate mecked modules"
     ,?_assert(MeckValidate)
     }
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_payment_token() -> [{string(), boolean()}].
check_payment_token() ->
    Context = cb_context:set_resp_data(cb_context:new(), []),
    Mod = 'kz_services_payment_tokens',
    ToMeck = [Mod, 'kz_services', 'kapps_util'],
    lists:foreach(fun(M) -> meck:new(M, ['passthrough']) end, ToMeck),

    %% simulate account has no plans
    meck:expect('kapps_util', 'get_master_account_id', fun() -> kz_binary:rand_hex(16) end),
    meck:expect('kz_services', 'fetch', fun(_) -> kz_services:empty() end),

    %% Account has service plans assigned and doesn't have any default payment token.
    meck:expect(Mod, 'defaults', fun(_) -> default_payment_tokens([]) end),
    Context3 = cb_alerts:check_payment_token(Context),
    [Alert] = cb_context:resp_data(Context3),

    %% Account has service plans assigned and also it has 2 default payment tokens not
    %% expired nor about to expire.
    Defaults1 = [payment_token(), payment_token()],
    meck:expect(Mod, 'defaults', fun(_) -> default_payment_tokens(Defaults1) end),
    Context4 = cb_alerts:check_payment_token(Context),

    %% Account has service plans assigned and also it has 2 default payments tokens from
    %% which 1 of those tokens has expired.
    Defaults2 = [expired_payment_token(), payment_token()],
    meck:expect(Mod, 'defaults', fun(_) -> default_payment_tokens(Defaults2) end),
    Context5 = cb_alerts:check_payment_token(Context),
    [Alert1] = cb_context:resp_data(Context5),

    %% Account has service plans assigned and also it has 2 default payments tokens from
    %% which 1 of those tokens is about to expire.
    Defaults3 = [payment_token(), about_to_expire_payment_token()],
    meck:expect(Mod, 'defaults', fun(_) -> default_payment_tokens(Defaults3) end),
    Context6 = cb_alerts:check_payment_token(Context),
    [Alert2] = cb_context:resp_data(Context6),

    %% Account has service plans assigned and also it has 2 default payments tokens from
    %% which 1 of those tokens is about to expire and the other one already expired.
    Defaults4 = [expired_payment_token(), about_to_expire_payment_token()],
    meck:expect(Mod, 'defaults', fun(_) -> default_payment_tokens(Defaults4) end),
    Context7 = cb_alerts:check_payment_token(Context),
    [Alert3, Alert4] = cb_context:resp_data(Context7),

    MeckValidate = lists:all(fun(Mecked) -> meck:validate(Mecked) end, ToMeck),
    lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

    [{"If account has service plans assigned and doesn't have any default payment tokens raise an alert"
     ,?_assertEqual(<<"no_payment_token">>, category_from_alert(Alert))
     }
    ,{"If account has service plans assigned and default payment tokens are not "
      ++ "expired nor about to expire the context should not change"
     ,?_assertEqual(Context4, Context)
     }
    ,{"If account has service plans assigned + default payment tokens configured and "
      ++ "any of those tokens has expired or is about to expire raise an alert"
     ,?_assertEqual(<<"expired_payment_token">>, category_from_alert(Alert1))
     }
    ,{"If account has service plans assigned + default payment tokens configured and "
      ++ "any of those tokens has expired or is about to expire raise an alert"
     ,?_assertEqual(<<"expired_payment_token">>, category_from_alert(Alert2))
     }
    ,{"If account has service plans assigned + default payment tokens configured and "
      ++ "any of those tokens has expired or is about to expire raise an alert"
     ,?_assertEqual({<<"expired_payment_token">>, <<"expired_payment_token">>},
                    {category_from_alert(Alert3), category_from_alert(Alert4)})
     }
    ,{"Validate mecked modules"
     ,?_assert(MeckValidate)
     }
    ].

%%%=======================================================================================
%%% Internal (helpers)
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_active_ports() -> {'ok', [kzd_port_requests:doc()]}.
account_active_ports() ->
    {'ok', [example_port_request(), unconfirmed_port_request(), rejected_port_request()]}.

-spec unconfirmed_port_request() -> kzd_port_requests:doc().
unconfirmed_port_request() ->
    kz_json:set_value(<<"pvt_port_state">>, <<"unconfirmed">>, example_port_request()).

-spec rejected_port_request() -> kzd_port_requests:doc().
rejected_port_request() ->
    kz_json:set_value(<<"pvt_port_state">>, <<"unconfirmed">>, example_port_request()).

-spec swap_comments(kzd_port_requests:doc()) -> kzd_port_requests:doc().
swap_comments(Port) ->
    %% Comment2 is the one with `action_required=true'
    [Comment1, Comment2] = kzd_port_requests:comments(Port),
    kzd_port_requests:set_comments(Port, [Comment2, Comment1]).

-spec add_comments(kzd_port_requests:doc()) -> kzd_port_requests:doc().
add_comments(Port) ->
    kzd_port_requests:set_comments(Port, [first_comment(), second_comment()]).

-spec first_comment() -> kz_json:object().
first_comment() ->
    kz_json:from_list(
      [{<<"author">>,<<"Someone">>}
      ,{<<"content">>,<<"First comment">>}
      ,{<<"timestamp">>,63709957526}
      ]).

-spec second_comment() -> kz_json:object().
second_comment() ->
    kz_json:from_list(
      [{<<"author">>,<<"Someone">>}
      ,{<<"content">>,<<"Second comment">>}
      ,{<<"timestamp">>,63709957539}
      ,{<<"action_required">>, 'true'}
      ]).

-spec category_from_alert(kz_json:object()) -> kz_term:ne_binary().
category_from_alert(Alert) ->
    kz_json:get_value(<<"category">>, Alert).

-spec state_from_port_alert(kz_json:object()) -> kz_term:ne_binary().
state_from_port_alert(Alert) ->
    kz_json:get_value([<<"metadata">>, <<"state">>], Alert).

-spec is_port_suspended_state(kz_term:ne_binary()) -> boolean().
is_port_suspended_state(State) ->
    lists:member(State, ?PORT_SUSPENDED_STATES).

-spec available_from_low_balance_alert(kz_json:object()) -> float().
available_from_low_balance_alert(Alert) ->
    kz_json:get_value([<<"metadata">>, <<"available">>], Alert).

-spec threshold_from_low_balance_alert(kz_json:object()) -> float() | 'undefined'.
threshold_from_low_balance_alert(Alert) ->
    kz_json:get_value([<<"metadata">>, <<"threshold">>], Alert).

-spec limits_enabled_postpay(pos_integer()) -> kz_json:object().
limits_enabled_postpay(MaxPostPayAmount) ->
    kz_json:set_values([{<<"pvt_allow_postpay">>,'true'}
                       ,{<<"pvt_max_postpay_amount">>,MaxPostPayAmount}
                       ]
                      ,limits()
                      ).

-spec payment_token() -> kz_json:object().
payment_token() ->
    Id = kz_binary:rand_hex(5),
    Props = [{<<"id">>, Id}
            ,{<<"default">>, 'true'}
             %% If expiration > kz_time:now_s() + 60 days then it is not expired.
            ,{<<"expiration">>, kz_time:now_s() + (?SECONDS_IN_DAY * 61)}
            ,{<<"created">>, kz_time:now_s()}
            ,{<<"modified">>, kz_time:now_s()}
            ],
    {Id, kz_json:from_list(Props)}.

-spec expired_payment_token() -> kz_json:object().
expired_payment_token() ->
    {TokenId, Token} = payment_token(),
    Expiration = kz_time:now_s() - ?SECONDS_IN_DAY, %% Expired 1 day ago.
    {TokenId, kz_json:set_value(<<"expiration">>, Expiration, Token)}.

-spec about_to_expire_payment_token() -> kz_json:object().
about_to_expire_payment_token() ->
    {TokenId, Token} = payment_token(),
    Expiration = kz_time:now_s() + (?SECONDS_IN_DAY * 30), %% Will expire in 30 days.
    {TokenId, kz_json:set_value(<<"expiration">>, Expiration, Token)}.

-spec default_payment_tokens(kz_json:objects()) -> kz_json:object().
default_payment_tokens(Tokens) ->
    kz_json:from_list(Tokens).

-spec limits() -> kz_json:object().
limits() ->
    kz_json:from_map(
      #{<<"_id">> => <<"limits">>,
        <<"_rev">> => <<"1-1397a5be21ce5cd08e4c7e32ef94e63c">>,
        <<"pvt_account_db">> => <<"account%2F8a%2F08%2F9c2a7e6c77be2e2e68c5c366f460">>,
        <<"pvt_account_id">> => <<"8a089c2a7e6c77be2e2e68c5c366f460">>,
        <<"pvt_cache_origins">> =>
            [{db,<<"account%2F8a%2F08%2F9c2a7e6c77be2e2e68c5c366f460">>, <<"limits">>}
            ,{db,<<"services">>,<<"8a089c2a7e6c77be2e2e68c5c366f460">>}
            ],
        <<"pvt_created">> => 63691458866,
        <<"pvt_modified">> => 63691458866,
        <<"pvt_type">> => <<"limits">>,
        <<"pvt_vsn">> => 1}
     ).

-spec example_port_request() -> kzd_port_requests:doc().
example_port_request() ->
    PortRequest =
        #{<<"_attachments">> =>
              #{<<"bill.pdf">> =>
                    #{<<"content_type">> => <<"application/pdf">>,
                      <<"digest">> => <<"md5-XeHinFwgbWai0D0yvVWJpQ==">>,
                      <<"length">> => 111111,<<"revpos">> => 2,<<"stub">> => true},
                <<"form.pdf">> =>
                    #{<<"content_type">> => <<"application/pdf">>,
                      <<"digest">> => <<"md5-i+NLkd4CfesS5i+SYEdWLm==">>,
                      <<"length">> => 222222,<<"revpos">> => 3,<<"stub">> => true}},
          <<"_id">> => <<"911bb8e73724fe9b06a7e1e3e3176c7e">>,
          <<"_rev">> => <<"9-c0f396d704a3679064b59cfbf5171ab2">>,
          <<"bill">> =>
              #{<<"account_number">> => <<>>,
                <<"btn">> => <<>>,
                <<"carrier">> => <<"2600hz">>,
                <<"locality">> => <<"San Francisco">>,
                <<"name">> => <<"2600Hz Inc.">>,
                <<"pin">> => <<>>,
                <<"postal_code">> => <<"00000">>,
                <<"region">> => <<"CA">>,
                <<"street_address">> => <<"140 Geary Street">>},
          <<"name">> => <<"Test cb_alerts">>,
          <<"notifications">> =>
              #{<<"email">> => #{<<"send_to">> => <<"email@example.com">>}},
          <<"numbers">> => #{<<"+12345678901">> => #{<<"used_by">> => <<"callflow">>}},
          <<"port_state">> => <<"submitted">>,
          <<"pvt_account_db">> => <<"port_requests">>,
          <<"pvt_account_id">> => <<"8a089c2a7e6c77be2e2e68c5c366f460">>,
          <<"pvt_alphanum_name">> => <<"testcbalerts">>,
          <<"pvt_auth_account_id">> => <<"8a089c2a7e6c77be2e2e68c5c366f460">>,
          <<"pvt_auth_user_id">> => <<"e8701ad48ba05a91604e480dd60899a3">>,
          <<"pvt_created">> => 63689901514,
          <<"pvt_is_authenticated">> => true,
          <<"pvt_modified">> => 63709957339,
          <<"pvt_port_state">> => <<"submitted">>,
          <<"pvt_request_id">> => <<"f68d2c3658a26018e43729b214bc84c9">>,
          <<"pvt_transitions">> =>
              [#{<<"authorization">> =>
                     #{<<"account">> =>
                           #{<<"id">> => <<"8a089c2a7e6c77be2e2e68c5c366f460">>,
                             <<"name">> => <<"Harry">>},
                       <<"user">> =>
                           #{<<"first_name">> => <<"Account">>,
                             <<"id">> => <<"e8701ad48ba05a91604e480dd60899a3">>,
                             <<"last_name">> => <<"Admin">>}},
                 <<"timestamp">> => 63689901515,
                 <<"transition">> =>
                     #{<<"new">> => <<"submitted">>,
                       <<"previous">> => <<"unconfirmed">>},
                 <<"type">> => <<"transition">>},
               #{<<"authorization">> =>
                     #{<<"account">> =>
                           #{<<"id">> => <<"8a089c2a7e6c77be2e2e68c5c366f460">>,
                             <<"name">> => <<"Harry">>},
                       <<"user">> =>
                           #{<<"first_name">> => <<"Account">>,
                             <<"id">> => <<"e8701ad48ba05a91604e480dd60899a3">>,
                             <<"last_name">> => <<"Admin">>}},
                 <<"timestamp">> => 63689901514,
                 <<"transition">> => #{<<"new">> => <<"unconfirmed">>},
                 <<"type">> => <<"transition">>}],
          <<"pvt_tree">> => <<>>,
          <<"pvt_type">> => <<"port_request">>,
          <<"pvt_vsn">> => <<"1">>,
          <<"transfer_date">> => 63690210000,
          <<"ui_flags">> => #{<<"type">> => <<"local">>,<<"validation">> => true},
          <<"ui_metadata">> =>
              #{<<"origin">> => <<"common">>,
                <<"ui">> => <<"monster-ui">>,
                <<"version">> => <<"4.3.0">>}},
    kz_json:from_map(PortRequest).
