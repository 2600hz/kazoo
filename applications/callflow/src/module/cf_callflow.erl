%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_callflow).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case whapps_call_command:b_channel_status(Call) of
        {'ok', _} -> maybe_branch_callflow(Data, Call);
        {'error', _} ->
            lager:debug("refusing to branch callflow for non-exstant call", []),
            cf_exe:stop(Call)
    end.

-spec maybe_branch_callflow(wh_json:object(), whapps_call:call()) -> 'ok'.
maybe_branch_callflow(Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(whapps_call:account_db(Call), Id) of
        {'error', R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call);
        {'ok', JObj} ->
            case whapps_call_command:b_channel_status(Call) of
                {'error', 'no_channel_id'} ->
                    lager:info("no channel up for ~s stopping", [Id]),
                    cf_exe:hard_stop(Call);
                'ok' ->
                    lager:info("branching to callflow ~s", [Id]),
                    Flow = wh_json:get_value(<<"flow">>, JObj, wh_json:new()),
                    cf_exe:branch(Flow, Call)
            end
    end.
