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

-spec post_bridge_actions() -> ne_binaries().
post_bridge_actions() ->
    [{<<"park">>,<<"Park-After-Pickup">>}
     ,{<<"transfer">>, <<"Transfer-After-Pickup">>}
     ,{<<"hangup">>, <<"Hangup-After-Pickup">>}
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    PostBridgeAction = wh_json:get_value(<<"action">>, Data),
    PostBridgeData = wh_json:get_value(<<"data">>, Data),
    case props:get_value(PostBridgeAction, post_bridge_actions(), 'undefined') of
        'undefined' ->
            lager:info("Can't found action, skipping"),
            'ok';
        Action ->
            lager:info("Got action ~s", [Action]),
            whapps_call_command:set(wh_json:from_list([{Action, PostBridgeData}]), wh_json:new(), Call)
    end,
    cf_exe:continue(Call).
