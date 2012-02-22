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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> ok.
handle(_, Call) ->
    CaptureGroup = whapps_call:kvs_fetch(cf_capture_group, Call),
    AccountId = whapps_call:account_id(Call),
    case is_binary(CaptureGroup) andalso cf_util:lookup_callflow(CaptureGroup, AccountId) of
        {ok, Flow, false} ->
            cf_call_command:set(undefined, wh_json:from_list([{<<"Auto-Answer">>, <<"true">>}]), Call),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow, wh_json:new()), Call);
        _ ->
            cf_exe:continue(Call)
    end.
