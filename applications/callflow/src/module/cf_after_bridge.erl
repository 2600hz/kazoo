%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%% set [park|transfer|hangup]_after_bridge variable
%%%
%%% "data":{
%%%   "action": "park" | "transfer" | "hangup",
%%%   "data":"some_extension_number" // number to transfer to or bool
%%% }
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------

-module(cf_after_bridge).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    PostBridgeAction = wh_json:get_value(<<"action">>, Data),
    PostBridgeData = wh_json:get_value(<<"data">>, Data),
    lager:info("injecting ~s(~p)", [PostBridgeAction, PostBridgeData]),
    Action = build_action(PostBridgeAction, PostBridgeData),
    whapps_call_command:set(Action, wh_json:new(), Call),
    cf_exe:continue(Call).

-spec build_action(ne_binary(), ne_binary()) -> wh_json:object().
build_action(<<"park">>, ShouldPark) ->
    wh_json:from_list([{<<"Park-After-Pickup">>, wh_util:is_true(ShouldPark)}]);
build_action(<<"hangup">>, ShouldHangup) ->
    wh_json:from_list([{<<"Hangup-After-Pickup">>, wh_util:is_true(ShouldHangup)}]);
build_action(<<"transfer">>, ToExtension) when is_binary(ToExtension) ->
    wh_json:from_list([{<<"Transfer-After-Pickup">>, ToExtension}]);
build_action(_Cmd, _Data) ->
    lager:info("unknown command: ~s(~p)", [_Cmd, _Data]),
    wh_json:new().
