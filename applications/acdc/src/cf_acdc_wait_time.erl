%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc
%%% @doc Data: {
%%%   "id":"queue id",
%%%   "window":900 // Window over which average wait time is calc'd
%%% }
%%%
%%%
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_wait_time).

-export([handle/2]).

-include_lib("callflow/src/callflow.hrl").

%%------------------------------------------------------------------------------
%% Handle execution of this callflow module
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    QueueId = kz_doc:id(Data),
    Skills = maybe_include_skills(QueueId, Call),
    Window = kz_json:get_integer_value(<<"window">>, Data),

    case Skills of
        'undefined' -> 'ok';
        _ -> lager:info("evaluating average wait time for skill set ~p", [Skills])
    end,

    case Window of
        'undefined' -> 'ok';
        _ -> lager:info("evaluating average wait time over last ~b seconds", [Window])
    end,

    Req = props:filter_undefined(
            [{<<"Account-ID">>, AccountId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Skills">>, Skills}
            ,{<<"Window">>, Window}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_stats:publish_average_wait_time_req/1
                            ,fun kapi_acdc_stats:average_wait_time_resp_v/1
                            )
    of
        {'ok', Resp} ->
            AverageWaitTime = kz_json:get_integer_value(<<"Average-Wait-Time">>, Resp, 0),
            lager:info("average wait time for account ~s queue ~s is ~B seconds", [AccountId, QueueId, AverageWaitTime]),
            {'branch_keys', BranchKeys} = cf_exe:get_branch_keys(Call),
            evaluate_average_wait_time(AverageWaitTime, BranchKeys, Call);
        {'error', E} ->
            lager:error("could not fetch average wait time for account ~s queue ~s: ~p", [AccountId, QueueId, E]),
            cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc If the selected strategy on the requested queue is skills-based
%% round robin, skills should be considered in the wait time eval.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_include_skills(kz_term:ne_binary(), kapps_call:call()) -> api_kz_term:ne_binaries().
maybe_include_skills(QueueId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, QueueId),
    case kz_json:get_ne_binary_value(<<"strategy">>, JObj) of
        <<"skills_based_round_robin">> ->
            kapps_call:kvs_fetch('acdc_required_skills', [], Call);
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc Continue to the branch of the callflow with the highest exceeded
%% threshold
%% @end
%%------------------------------------------------------------------------------
-spec evaluate_average_wait_time(non_neg_integer(), kz_json:path(), kapps_call:call()) -> 'ok'.
evaluate_average_wait_time(AverageWaitTime, Keys, Call) ->
    Keys1 = lists:sort(fun(Key1, Key2) ->
                               kz_term:to_integer(Key1) >= kz_term:to_integer(Key2)
                       end, Keys),
    evaluate_average_wait_time2(AverageWaitTime, Keys1, Call).

-spec evaluate_average_wait_time2(non_neg_integer(), kz_json:path(), kapps_call:call()) -> 'ok'.
evaluate_average_wait_time2(_, [], Call) ->
    cf_exe:continue(Call);
evaluate_average_wait_time2(AverageWaitTime, [Key|Keys], Call) ->
    Threshold = kz_term:to_integer(Key),
    case AverageWaitTime >= Threshold of
        'true' ->
            lager:info("average wait time exceeded threshold ~B", [Threshold]),
            cf_exe:continue(Key, Call);
        'false' ->
            evaluate_average_wait_time2(AverageWaitTime, Keys, Call)
    end.
