%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_intercom).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> ok.
handle(_, #cf_call{capture_group=undefined}=Call) ->
    cf_exe:continue(Call);
handle(_, #cf_call{capture_group=Digits, account_id=AccountId}=Call) ->
    cf_call_command:set(undefined, {struct, [{<<"Auto-Answer">>, <<"true">>}]}, Call),
    case cf_util:lookup_callflow(Digits, AccountId) of
        {ok, Flow, false} ->
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow, wh_json:new()), Call);
        _ ->
            cf_exe:continue(Call)
    end.
