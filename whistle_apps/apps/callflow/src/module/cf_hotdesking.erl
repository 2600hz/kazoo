%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_hotdesking.

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) ->
		        no_return()).
handle(Data, #cf_call{call_id=CallId, account_db=AccountDb, cf_pid=CFPid}) ->

    put(callid, CallId),
    Id = wh_json:get_value(<<"id">>, Data),
        case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj} ->

		?LOG("branching to callflow ~s", [Id]),
		Flow = wh_json:get_value(<<"flow">>, JObj, ?EMPTY_JSON_OBJECT),
		CFPid ! {branch, Flow};
	            {error, R} ->
            ?LOG("could not branch to callflow ~s, ~p", [Id, R]),
            {continue}
    end.
