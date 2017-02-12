%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_callflow).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Id = kz_json:get_ne_binary_value(<<"id">>, Data),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Id) of
        {'error', R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call);
        {'ok', JObj} ->
            lager:info("branching to new callflow ~s", [Id]),
            Flow = kzd_callflow:flow(JObj, kz_json:new()),
            cf_exe:branch(Flow, update_call(JObj, Call))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add information to CCVs, making call logs more useful
%% @end
%%--------------------------------------------------------------------
-spec update_call(kz_json:object(), kapps_call:call()) -> kapps_call:call().
update_call(JObj, Call) ->
    Updaters = [fun remove_old_values/1
               ,{fun kapps_call:set_branched_callflow_id/2, kz_doc:id(JObj)}
               ,{fun maybe_add_group_id/2, JObj}
               ],
    NewCall = kapps_call:exec(Updaters, Call),
    cf_exe:set_call(NewCall),
    NewCall.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set the group_id field in the CCVs if the callflow is a baseGroup
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_group_id(kz_json:object(), kapps_call:call()) -> kapps_call:call().
maybe_add_group_id(JObj, Call) ->
    case kz_json:get_value(<<"type">>, JObj) of
        <<"baseGroup">> ->
            GroupId = kz_json:get_value(<<"group_id">>, JObj),
            kapps_call:set_monster_group_id(GroupId, Call);
        _ ->
            Call
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Clear fields that are no longer true for the call
%% @end
%%--------------------------------------------------------------------
-spec remove_old_values(kapps_call:call()) -> kapps_call:call().
remove_old_values(Call) ->
    CCVsToRemove = [<<"Group-ID">>],
    kapps_call:remove_custom_channel_vars(CCVsToRemove, Call).
