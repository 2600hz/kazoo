%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Starts a {@link cf_exe} and processes the "flow" sans DB.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_resume).

-export([handle_req/2]).

-include("callflow.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> kapps_call:call().
handle_req(JObj, _Props) ->
    'true' = kapi_callflow:resume_v(JObj),
    Call0 = kapps_call:from_json(kz_json:get_json_value(<<"Call">>, JObj)),
    kapps_call:put_callid(Call0),
    lager:info("received call resume, taking control ~p", [Call0]),
    ControllerQ = list_to_binary(["pid://", kz_term:to_binary(self()), "/", props:get_value('queue', Props)]),
    Flow = kz_json:get_value(<<"Flow">>, JObj),
    Updaters = [{fun kapps_call:kvs_store/2, Flow}
               ,{fun kapps_call:set_controller_queue/2, ControllerQ}
               ],
    Call = kapps_call:exec(Updaters, Call).
    cf_util:flush_control_queue(Call),
    cf_route_win:execute_callflow(JObj, Call).
