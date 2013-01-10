%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2011 by Karl Anderson <karl@2600hz.org>
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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> ok.
handle(Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(whapps_call:account_db(Call), Id) of
        {ok, JObj} ->
            lager:info("branching to callflow ~s", [Id]),
            Flow = wh_json:get_value(<<"flow">>, JObj, wh_json:new()),
            cf_exe:branch(Flow, Call);
        {error, R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call)
    end.
