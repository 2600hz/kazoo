%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    PostBridgeAction = kz_json:get_value(<<"action">>, Data),
    PostBridgeData = kz_json:get_value(<<"data">>, Data),
    lager:info("injecting ~s(~p)", [PostBridgeAction, PostBridgeData]),
    Action = build_action(PostBridgeAction, PostBridgeData),
    kapps_call_command:set(Action, kz_json:new(), Call),
    cf_exe:continue(Call).

-spec build_action(ne_binary(), ne_binary()) -> kz_json:object().
build_action(<<"park">>, ShouldPark) ->
    kz_json:from_list([{<<"Park-After-Pickup">>, kz_util:is_true(ShouldPark)}]);
build_action(<<"hangup">>, ShouldHangup) ->
    kz_json:from_list([{<<"Hangup-After-Pickup">>, kz_util:is_true(ShouldHangup)}]);
build_action(<<"transfer">>, ToExtension) when is_binary(ToExtension) ->
    kz_json:from_list([{<<"Transfer-After-Pickup">>, ToExtension}]);
build_action(_Cmd, _Data) ->
    lager:info("unknown command: ~s(~p)", [_Cmd, _Data]),
    kz_json:new().
