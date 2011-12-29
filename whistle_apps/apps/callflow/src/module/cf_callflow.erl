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
-spec handle/2 :: (json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{account_db=AccountDb}=Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj} ->
            ?LOG("branching to callflow ~s", [Id]),
            Flow = wh_json:get_value(<<"flow">>, JObj, ?EMPTY_JSON_OBJECT),
            cf_exe:branch(Flow, Call);
        {error, R} ->
            ?LOG("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call)
    end.
