%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_acdc_wait_time).

-export([handle/2]).

-behaviour(gen_cf_action).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% Handle execution of this callflow module
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    QueueId = maybe_use_variable(Data, Call),

    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Queue-ID">>, QueueId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_request(Req
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a variable id is specified and refers to an existing document,
%% use that variable id instead of the one specified in the module's
%% data
%% @end
%%--------------------------------------------------------------------
-spec maybe_use_variable(kz_json:object(), kapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"var">>, Data) of
        'undefined' ->
            kz_json:get_ne_binary_value(<<"id">>, Data);
        Variable ->
            Value = kz_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> kz_doc:id(Data)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Continue to the branch of the callflow with the highest exceeded
%% threshold
%% @end
%%--------------------------------------------------------------------
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
