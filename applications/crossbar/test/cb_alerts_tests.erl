-module(cb_alerts_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=======================================================================================
%%% Tests generator
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cb_alerts_test_() -> [{string(), boolean()}].
cb_alerts_test_() ->
    check_port_request_status()
    ++ check_port_request_comments()
    ++ check_low_balance().

%%%=======================================================================================
%%% Tests
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_port_request_status() -> [{string(), boolean()}].
check_port_request_status() ->
    Mod = 'knm_port_request',
    meck:new(Mod, [passthrough]),

    Context = cb_context:new(),

    %% 1 submitted, 1 scheduled, and 1 rejected.
    meck:expect(Mod, 'account_active_ports', fun(_) -> account_active_ports() end),
    Context1 = cb_alerts:check_port_request_status(cb_context:new()),

    %% Not active ports found.
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'error', 'not_found'} end),
    Context2 = cb_alerts:check_port_request_status(Context),

    %% Ports without scheduled or rejected state.
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'ok', [example_port_request()]} end),
    Context3 = cb_alerts:check_port_request_status(Context),

    meck:unload(Mod),

    [{"Only scheduled and rejected port requests are returned"
     ,?_assertEqual(2, length(cb_context:resp_data(Context1)))
     }
    ,{"When not active ports are found the context should not change"
     ,?_assertEqual(Context2, Context)
     }
    ,{"When not scheduled or rejected ports found the context should not change"
     ,?_assertEqual(Context3, Context)
     }
    ].

-spec check_port_request_comments() -> [{string(), boolean()}].
check_port_request_comments() ->
    Mod = 'knm_port_request',
    meck:new(Mod, [passthrough]),

    Context = cb_context:new(),

    %% All ports (3) have last comment with `waiting_for_reply=true' within the last comment
    meck:expect(Mod, 'account_active_ports', fun(_) -> account_active_ports() end),
    Context1 = cb_alerts:check_port_request_comments(Context),

    %% Only 1 port with `waiting_for_reply=true' within the last comment
    Port = swap_comments(example_port_request()), %% Last comment doesn't have `waiting_for_reply=true'
    Port1 = swap_comments(scheduled_port_request()), %% Last comment doesn't have `waiting_for_reply=true'
    F = fun(_) -> {'ok', [Port, Port1, rejected_port_request()]} end,
    meck:expect(Mod, 'account_active_ports', F),
    Context2 = cb_alerts:check_port_request_comments(Context),

    %% Not comments with `waiting_for_reply=true' within the last comment
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'ok', [Port]} end),
    Context3 = cb_alerts:check_port_request_comments(Context),

    %% Not active ports found
    meck:expect(Mod, 'account_active_ports', fun(_) -> {'error', 'not_found'} end),
    Context4 = cb_alerts:check_port_request_comments(Context),

    meck:unload(Mod),

    [{"Only return ports with 'waiting for reply=true' within the last comment"
     ,?_assertEqual(3, length(cb_context:resp_data(Context1)))
     }
    ,{"Only return ports with 'waiting for reply=true' within the last comment"
     ,?_assertEqual(1, length(cb_context:resp_data(Context2)))
     }
    ,{"If not ports with 'waiting for reply=true' within the last comment the context should not change"
     ,?_assertEqual(Context3, Context)
     }
    ,{"If not active ports found the context should not change"
     ,?_assertEqual(Context4, Context)
     }
    ].

-spec check_low_balance() -> [{string(), boolean()}].
check_low_balance() ->
    Context = cb_context:new(),
    ThresholdUSD = 5.0,
    F = fun(Available, Threshold) ->
            kz_json:from_list([{<<"available_dollars">>, Available}
                              ,{<<"threshold">>, Threshold}
                              ,{<<"type">>, <<"balance_is_below_zero">>}
                              ])
        end,
    MaybeTopupResult = {'ok', kz_transaction:empty(), 'undefined'},

    Mod = 'kz_currency',
    ToMeck = [Mod, 'kzd_accounts', 'kz_services_topup'],
    lists:foreach(fun(M) -> meck:new(M, ['passthrough']) end, ToMeck),

    meck:expect('kzd_accounts', 'low_balance_threshold', fun(_) -> ThresholdUSD end),

    %% timeout trying to get account's available units
    meck:expect(Mod, 'available_dollars', fun(_) -> {'error', 'timeout'} end),
    Context1 = cb_alerts:check_low_balance(Context),

    %% error /= 'timeout' trying to get account's available units
    meck:expect(Mod, 'available_dollars', fun(_) -> {'error', 'anything'} end),
    Context2 = cb_alerts:check_low_balance(Context),

    %% available dollars > Threshold dollars
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD + 1.0} end),
    Context3 = cb_alerts:check_low_balance(Context),

    %% available dollars = Threshold dollars
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD} end),
    Context4 = cb_alerts:check_low_balance(Context),
    [RespObj] = cb_context:resp_data(Context4),

    %% available dollars < Threshold dollars
    AD = ThresholdUSD - 1.0,
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', AD} end),
    Context5 = cb_alerts:check_low_balance(Context),
    [RespObj1] = cb_context:resp_data(Context5),
    %% topup succeeds
    meck:expect('kz_services_topup', maybe_topup, fun(_, _) -> MaybeTopupResult end),
    Context6 = cb_alerts:check_low_balance(Context),

    %% Unload mecked modules
    lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

    [{"If getting account's available units fails with 'timeout' 3 times in a row the context should not change"
     ,?_assertEqual(Context1, Context)
     }
    ,{"If getting account's available units fails the context should not change"
     ,?_assertEqual(Context2, Context)
     }
    ,{"If available dollars > Threshold dollars the context should not change"
     ,?_assertEqual(Context3, Context)
     }
    ,{"When available dolars = Threshold dollars return alert"
     ,?_assertEqual('true', kz_json:are_equal(F(ThresholdUSD, ThresholdUSD), RespObj))
     }
    ,{"When available dolars < Threshold dollars return alert"
     ,?_assertEqual('true', kz_json:are_equal(F(AD, ThresholdUSD), RespObj1))
     }
    ,{"When available dolars < Threshold dollars but topup succeeds the context should not change"
     ,?_assertEqual(Context6, Context)
     }
    ].

%%%=======================================================================================
%%% Internal (helpers)
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_active_ports() -> {'ok', kz_json:objects()}.
account_active_ports() ->
    {'ok', [example_port_request(), scheduled_port_request(), rejected_port_request()]}.

-spec scheduled_port_request() -> kz_json:object().
scheduled_port_request() ->
    kz_json:set_value(<<"pvt_port_state">>, <<"scheduled">>, example_port_request()).

-spec rejected_port_request() -> kz_json:object().
rejected_port_request() ->
    kz_json:set_value(<<"pvt_port_state">>, <<"rejected">>, example_port_request()).

-spec swap_comments(kz_json:object()) -> kz_json:object().
swap_comments(Port) ->
    %% Comment2 is the one with `waiting_for_reply=true'
    [Comment1, Comment2] = kz_json:get_value(<<"comments">>, Port),
    kz_json:set_value(<<"comments">>, [Comment2, Comment1], Port).

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
        ,{<<"waiting_for_reply">>, 'true'}
        ]).

-spec example_port_request() -> kz_json:object().
example_port_request() ->
    {[{<<"_attachments">>,
       {[{<<"bill.pdf">>,
          {[{<<"content_type">>,<<"application/pdf">>},
            {<<"digest">>,<<"md5-XeHinFwgbWai0D0yvVWJpQ==">>},
            {<<"length">>,111111},
            {<<"revpos">>,2},
            {<<"stub">>,true}]}},
         {<<"form.pdf">>,
          {[{<<"content_type">>,<<"application/pdf">>},
            {<<"digest">>,<<"md5-i+NLkd4CfesS5i+SYEdWLm==">>},
            {<<"length">>,222222},
            {<<"revpos">>,3},
            {<<"stub">>,true}]}}]}},
      {<<"_id">>,<<"911bb8e73724fe9b06a7e1e3e3176c7e">>},
      {<<"_rev">>,<<"9-c0f396d704a3679064b59cfbf5171ab2">>},
      {<<"bill">>,
       {[{<<"account_number">>,<<>>},
         {<<"btn">>,<<>>},
         {<<"carrier">>,<<"2600hz">>},
         {<<"locality">>,<<"San Francisco">>},
         {<<"name">>,<<"2600Hz Inc.">>},
         {<<"pin">>,<<>>},
         {<<"postal_code">>,<<"00000">>},
         {<<"region">>,<<"CA">>},
         {<<"street_address">>,<<"140 Geary Street">>}]}},
      {<<"comments">>, [first_comment(), second_comment()]},
      {<<"name">>,<<"Test cb_alerts">>},
      {<<"notifications">>,
       {[{<<"email">>,{[{<<"send_to">>,<<"email@example.com">>}]}}]}},
      {<<"numbers">>,{[{<<"+12345678901">>,{[{<<"used_by">>,<<"callflow">>}]}}]}},
      {<<"port_state">>,<<"unconfirmed">>},
      {<<"pvt_account_db">>,<<"port_requests">>},
      {<<"pvt_account_id">>,<<"8a089c2a7e6c77be2e2e68c5c366f460">>},
      {<<"pvt_alphanum_name">>,<<"testcbalerts">>},
      {<<"pvt_auth_account_id">>,<<"8a089c2a7e6c77be2e2e68c5c366f460">>},
      {<<"pvt_auth_user_id">>,<<"e8701ad48ba05a91604e480dd60899a3">>},
      {<<"pvt_created">>,63689901514},
      {<<"pvt_is_authenticated">>,true},
      {<<"pvt_modified">>,63709957339},
      {<<"pvt_port_state">>,<<"submitted">>},
      {<<"pvt_request_id">>,<<"f68d2c3658a26018e43729b214bc84c9">>},
      {<<"pvt_transitions">>,
       [{[{<<"authorization">>,
           {[{<<"account">>,
              {[{<<"id">>,<<"8a089c2a7e6c77be2e2e68c5c366f460">>},
                {<<"name">>,<<"Harry">>}]}},
             {<<"user">>,
              {[{<<"first_name">>,<<"Account">>},
                {<<"id">>,<<"e8701ad48ba05a91604e480dd60899a3">>},
                {<<"last_name">>,<<"Admin">>}]}}]}},
          {<<"timestamp">>,63689901515},
          {<<"transition">>,
           {[{<<"new">>,<<"submitted">>},{<<"previous">>,<<"unconfirmed">>}]}},
          {<<"type">>,<<"transition">>}]},
        {[{<<"authorization">>,
           {[{<<"account">>,
              {[{<<"id">>,<<"8a089c2a7e6c77be2e2e68c5c366f460">>},
                {<<"name">>,<<"Harry">>}]}},
             {<<"user">>,
              {[{<<"first_name">>,<<"Account">>},
                {<<"id">>,<<"e8701ad48ba05a91604e480dd60899a3">>},
                {<<"last_name">>,<<"Admin">>}]}}]}},
          {<<"timestamp">>,63689901514},
          {<<"transition">>,{[{<<"new">>,<<"unconfirmed">>}]}},
          {<<"type">>,<<"transition">>}]}]},
      {<<"pvt_tree">>,[]},
      {<<"pvt_type">>,<<"port_request">>},
      {<<"pvt_vsn">>,<<"1">>},
      {<<"transfer_date">>,63690210000},
      {<<"ui_flags">>,{[{<<"type">>,<<"local">>},{<<"validation">>,true}]}},
      {<<"ui_metadata">>,
       {[{<<"origin">>,<<"common">>},
         {<<"ui">>,<<"monster-ui">>},
         {<<"version">>,<<"4.3.0">>}]}}]}.
