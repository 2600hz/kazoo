%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_hard_limit).

-export([is_under/2]).

-include("jonny5.hrl").

-spec is_under(#limits{}, wh_json:object()) -> boolean().
is_under(Limits, JObj) ->
    case calls_at_limit(Limits, JObj)
        orelse resource_consumption_at_limit(Limits, JObj)
    of
        'false' -> 'true';
        'true' ->
            j5_util:send_system_alert(<<"hard limit">>, JObj, Limits),
            'false'
    end.

-spec calls_at_limit(#limits{}, wh_json:object()) -> boolean().
calls_at_limit(#limits{calls=-1}, _) ->
    'false';
calls_at_limit(#limits{calls=0}, _) ->
    'true';
calls_at_limit(#limits{calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Calls">>], JObj, 0),
    Resources - ConsumedResources < 0.

-spec resource_consumption_at_limit(#limits{}, wh_json:object()) -> boolean().
resource_consumption_at_limit(#limits{resource_consuming_calls=-1}, _) ->
    'false';
resource_consumption_at_limit(#limits{resource_consuming_calls=0}, _) ->
    'true';
resource_consumption_at_limit(#limits{resource_consuming_calls=Resources}, JObj) ->
    ConsumedResources = wh_json:get_integer_value([<<"Usage">>, <<"Resource-Consuming-Calls">>], JObj, 0),
    Resources - ConsumedResources < 0.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

limits(C, R) ->
    #limits{calls = C
            ,resource_consuming_calls = R
           }.

authz_jobj(C, R) ->
    Props = [{<<"Calls">>, C}
             ,{<<"Resource-Consuming-Calls">>, R}
            ],
    wh_json:from_list([{<<"Usage">>, wh_json:from_list(Props)}]).

calls_at_limit_test() ->
    %% no calls, unlimited allowed
    ?assertEqual('false', calls_at_limit(limits(-1, 0), authz_jobj(0, 0))),
    %% no calls, none allowed
    ?assertEqual('true', calls_at_limit(limits(0, 0), authz_jobj(0, 0))),
    %% 1 call, none allowed
    ?assertEqual('true', calls_at_limit(limits(0, 0), authz_jobj(1, 0))),
    %% 1 call, 1 allowed
    ?assertEqual('false', calls_at_limit(limits(1, 0), authz_jobj(1, 0))),
    %% 2 calls, 1 allowed
    ?assertEqual('true', calls_at_limit(limits(1, 0), authz_jobj(2, 0))),
    %% 1 calls, 2 allowed
    ?assertEqual('false', calls_at_limit(limits(2, 0), authz_jobj(1, 0))),
    ok.

resource_consumption_at_limit_test() ->
    %% no calls, unlimited allowed
    ?assertEqual('false', resource_consumption_at_limit(limits(0, -1), authz_jobj(0, 0))),
    %% no calls, none allowed
    ?assertEqual('true', resource_consumption_at_limit(limits(0, 0), authz_jobj(0, 0))),
    %% 1 call, none allowed
    ?assertEqual('true', resource_consumption_at_limit(limits(0, 0), authz_jobj(0, 1))),
    %% 1 call, 1 allowed
    ?assertEqual('false', resource_consumption_at_limit(limits(0, 1), authz_jobj(0, 1))),
    %% 2 calls, 1 allowed
    ?assertEqual('true', resource_consumption_at_limit(limits(0, 1), authz_jobj(0, 2))),
    %% 1 calls, 2 allowed
    ?assertEqual('false', resource_consumption_at_limit(limits(0, 2), authz_jobj(0, 1))),
    ok.

is_under_test() ->
    %% no calls, unlimited allowed
    ?assertEqual('true', is_under(limits(-1, -1), authz_jobj(0, 0))),
    %% limited by calls
    ?assertEqual('false', is_under(limits(0, -1), authz_jobj(0, 0))),
    %% limited by resources
    ?assertEqual('false', is_under(limits(-1, 0), authz_jobj(0, 0))),
    ok.

-endif.
