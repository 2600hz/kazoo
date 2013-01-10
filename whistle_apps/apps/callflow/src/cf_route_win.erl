%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% handler for route wins, bootstraps callflow execution
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_route_win).

-include("callflow.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:object(), proplist()) -> any().
handle_req(JObj, _Options) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),

    lager:info("received route win"),
    case whapps_call:retrieve(CallId) of
        {ok, Call} ->
            lager:info("bootstrapping callflow executer"),
            bootstrap_callflow_executer(JObj, Call);
        {error, R} ->
            lager:info("unable to find callflow during second lookup (HUH?) ~p", [R])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec bootstrap_callflow_executer/2 :: (wh_json:object(), whapps_call:call()) -> {'ok', pid()}.
bootstrap_callflow_executer(JObj, Call) ->
    lists:foldl(fun(F, C) -> F(C) end
                ,whapps_call:from_route_win(JObj, Call)
                ,[fun store_owner_id/1
                  ,fun update_ccvs/1
                  %% all funs above here return whapps_call:call()
                  ,fun execute_callflow/1
                 ]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec store_owner_id/1 :: (whapps_call:call()) -> whapps_call:call().
store_owner_id(Call) ->
    OwnerId = cf_attributes:owner_id(Call),
    whapps_call:kvs_store(owner_id, OwnerId, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec update_ccvs/1 :: (whapps_call:call()) -> whapps_call:call().
update_ccvs(Call) ->
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    Props = props:filter_undefined([{<<"Hold-Media">>, cf_attributes:moh_attributes(<<"media_id">>, Call)}
                                    ,{<<"Caller-ID-Name">>, CIDName}
                                    ,{<<"Caller-ID-Number">>, CIDNumber}
                                   ]),
    whapps_call:set_custom_channel_vars(Props, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_callflow/1 :: (whapps_call:call()) -> {'ok', pid()}.
execute_callflow(Call) ->
    lager:info("call has been setup, passing control to callflow executer"),
    cf_exe_sup:new(Call).
