-module(cb_alerts_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

%%%=======================================================================================
%%% Tests generator
%%%=======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cb_alerts_test_() -> [{string(), boolean()}].
cb_alerts_test_() ->
    check_port_requests()
        ++ check_low_balance().

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

    _ = lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

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
    ].

-spec check_low_balance() -> [{string(), boolean()}].
check_low_balance() ->
    Context = cb_context:set_resp_data(cb_context:new(), []),
    ThresholdUSD = 5.0,
    Mod = 'kz_currency',
    ToMeck = [Mod, 'kzd_accounts'],

    lists:foreach(fun(M) -> meck:new(M, ['passthrough']) end, ToMeck),
    meck:expect('kzd_accounts', 'low_balance_threshold', fun(_) -> ThresholdUSD end),

    %% error trying to get account's available dollars
    meck:expect(Mod, 'available_dollars', fun(_) -> {'error', 'reason'} end),
    Context1 = cb_alerts:check_low_balance(Context),

    %% available dollars == Threshold dollars
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD} end),
    Context2 = cb_alerts:check_low_balance(Context),

    %% available dollars > Threshold dollars
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', ThresholdUSD + 1.0} end),
    Context3 = cb_alerts:check_low_balance(Context),

    %% available dollars < Threshold dollars
    AD = ThresholdUSD - 1.0,
    meck:expect(Mod, 'available_dollars', fun(_) -> {'ok', AD} end),
    Context4 = cb_alerts:check_low_balance(Context),
    [Alert1] = cb_context:resp_data(Context4),

    %% Unload mecked modules
    lists:foreach(fun(M) -> meck:unload(M) end, ToMeck),

    [{"If getting account's available units fails the context should not change"
     ,?_assertEqual(Context1, Context)
     }
    ,{"If available dollars == Threshold dollars the context should not change"
     ,?_assertEqual(Context2, Context)
     }
    ,{"If available dollars > Threshold dollars the context should not change"
     ,?_assertEqual(Context3, Context)
     }
    ,{"When available dolars < Threshold dollars return alert"
     ,?_assertEqual({AD, ThresholdUSD, <<"low_balance">>},
                    {kz_json:get_value([<<"metadata">>, <<"available">>], Alert1)
                    ,kz_json:get_value([<<"metadata">>, <<"threshold">>], Alert1)
                    ,category_from_alert(Alert1)
                    })
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
    kzd_port_requests:set_port_state(example_port_request(), <<"unconfirmed">>).

-spec rejected_port_request() -> kzd_port_requests:doc().
rejected_port_request() ->
    kzd_port_requests:set_port_state(example_port_request(), <<"rejected">>).

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

-spec example_port_request() -> kzd_port_requests:doc().
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
      {<<"name">>,<<"Test cb_alerts">>},
      {<<"notifications">>,
       {[{<<"email">>,{[{<<"send_to">>,<<"email@example.com">>}]}}]}},
      {<<"numbers">>,{[{<<"+12345678901">>,{[{<<"used_by">>,<<"callflow">>}]}}]}},
      {<<"port_state">>,<<"submitted">>},
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
