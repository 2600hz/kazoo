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
    Id = kz_doc:id(Data),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Id) of
        {'error', R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call);
        {'ok', JObj} ->
            lager:info("branching to new callflow ~s", [kz_doc:id(JObj)]),
            Flow = kz_json:get_value(<<"flow">>, JObj, kz_json:new()),
            cf_exe:branch(Flow, Call)
    end.
