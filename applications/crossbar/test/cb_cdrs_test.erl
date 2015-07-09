%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_cdrs_test).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_LEG, <<"{\"call_id\":\"single_a\",\"timestamp\":1,\"direction\":\"inbound\"}">>).

-define(NORMAL_A_LEG, <<"{\"call_id\":\"normal_a\",\"timestamp\":1,\"other_leg_call_id\":\"normal_b\",\"call_direction\":\"inbound\"}">>).
-define(NORMAL_B_LEG, <<"{\"call_id\":\"normal_b\",\"timestamp\":2,\"other_leg_call_id\":\"normal_a\",\"call_direction\":\"outbound\"}">>).

-define(BLIND_A_LEG, <<"{\"call_id\":\"blind_a\",\"timestamp\":1,\"other_leg_call_id\":\"blind_c\",\"call_direction\":\"inbound\"}">>).
-define(BLIND_B_LEG, <<"{\"call_id\":\"blind_b\",\"timestamp\":2,\"other_leg_call_id\":\"blind_a\",\"call_direction\":\"outbound\"}">>).
-define(BLIND_C_LEG, <<"{\"call_id\":\"blind_c\",\"timestamp\":3,\"other_leg_call_id\":\"blind_a\",\"call_direction\":\"inbound\"}">>).

-define(ATTX_A_LEG, <<"{\"call_id\":\"attx_a\",\"timestamp\":1,\"other_leg_call_id\":\"attx_d\",\"call_direction\":\"inbound\"}">>).
-define(ATTX_B_LEG, <<"{\"call_id\":\"attx_b\",\"timestamp\":2,\"other_leg_call_id\":\"attx_a\",\"call_direction\":\"outbound\"}">>).
-define(ATTX_C_LEG, <<"{\"call_id\":\"attx_c\",\"timestamp\":3,\"other_leg_call_id\":\"attx_d\",\"call_direction\":\"inbound\"}">>).
-define(ATTX_D_LEG, <<"{\"call_id\":\"attx_d\",\"timestamp\":4,\"other_leg_call_id\":\"attx_a\",\"call_direction\":\"outbound\"}">>).

-define(CDRS, [?SINGLE_LEG
               ,?NORMAL_B_LEG, ?NORMAL_A_LEG
               ,?BLIND_A_LEG, ?BLIND_B_LEG, ?BLIND_C_LEG
               ,?ATTX_A_LEG, ?ATTX_B_LEG, ?ATTX_C_LEG, ?ATTX_D_LEG
              ]).

group_by_calls_test() ->
    Legs = wh_util:shuffle_list(
             [wh_json:decode(Leg) || Leg <- ?CDRS]
            ),
    Start = os:timestamp(),
    wh_json:foreach(fun grouped_call/1
                    ,cb_cdrs:group_cdrs(Legs)
                   ),
    ?debugFmt("processing CDRs took ~p ms~n", [wh_util:elapsed_ms(Start)]).

grouped_call({<<"single_a">>, Legs}) ->
    ?assertEqual(1, length(Legs)),
    [A] = Legs,
    ?assertEqual(<<"single_a">>, kzd_cdr:call_id(A)),
    ?assertEqual('true', wh_json:is_true(<<"is_a_leg">>, A));
grouped_call({<<"normal_a">>, Legs}) ->
    ?assertEqual(2, length(Legs)),
    [A, B] = Legs,
    ?assertEqual(<<"normal_a">>, kzd_cdr:call_id(A)),
    ?assertEqual(<<"normal_b">>, kzd_cdr:call_id(B)),
    ?assertEqual('true', wh_json:is_true(<<"is_a_leg">>, A)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, B));
grouped_call({<<"blind_a">>, Legs}) ->
    ?assertEqual(3, length(Legs)),
    [A, B, C] = Legs,
    ?assertEqual(<<"blind_a">>, kzd_cdr:call_id(A)),
    ?assertEqual(<<"blind_b">>, kzd_cdr:call_id(B)),
    ?assertEqual(<<"blind_c">>, kzd_cdr:call_id(C)),

    ?assertEqual('true', wh_json:is_true(<<"is_a_leg">>, A)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, B)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, C));
grouped_call({<<"attx_a">>, Legs}) ->
    ?assertEqual(4, length(Legs)),
    [A, B, C, D] = Legs,
    ?assertEqual(<<"attx_a">>, kzd_cdr:call_id(A)),
    ?assertEqual(<<"attx_b">>, kzd_cdr:call_id(B)),
    ?assertEqual(<<"attx_c">>, kzd_cdr:call_id(C)),
    ?assertEqual(<<"attx_d">>, kzd_cdr:call_id(D)),

    ?assertEqual('true', wh_json:is_true(<<"is_a_leg">>, A)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, B)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, C)),
    ?assertEqual('false', wh_json:is_true(<<"is_a_leg">>, D)).
