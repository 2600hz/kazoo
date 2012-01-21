%%%-------------------------------------------------------------------
%%% @author Karl anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% handler for route wins, bootstraps callflow execution
%%% @end
%%% Created : 30 Nov 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_route_win).

-include("callflow.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> no_return().
handle_req(JObj, _Options) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),

    ?LOG_START("received route win"),

    {ok, #cf_call{authorizing_id=AuthId}=Call}
        = wh_cache:fetch({cf_call, CallId}),

    case cf_util:lookup_callflow(Call) of
        {ok, Flow, _} ->
            ?LOG("bootstrapping callflow executer"),
            OwnerId = cf_attributes:owner_id(AuthId, Call),
            {CIDNumber, CIDName} = cf_attributes:caller_id(AuthId, OwnerId, <<"external">>, Call),
            C = Call#cf_call{capture_group = wh_json:get_ne_value(<<"capture_group">>, Flow)
                             ,owner_id = OwnerId
                             ,cid_name = CIDName
                             ,cid_number = CIDNumber
                            },
            execute_call_flow(Flow, wh_json:get_value(<<"Control-Queue">>, JObj), CallId, C);
        {error, R} ->
            ?LOG_END("unable to find callflow during second lookup (HUH?) ~p", [R])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_call_flow/4 :: (wh_json:json_object(), ne_binary(), ne_binary(), #cf_call{}) -> 'ok'.
execute_call_flow(Flow, ControlQ, CallId, Call) ->
    CCVs = get_channel_ccvs(Call),
    ?LOG("call has been setup, passing control to callflow executer"),
    {ok, CFPid} = cf_exe_sup:new(wh_json:get_value(<<"flow">>, Flow), ControlQ, CallId, Call#cf_call{channel_vars=CCVs}),
    cf_call_command:set(CCVs, get_call_ccvs(Call), Call#cf_call{cf_pid=CFPid}),
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% get the custom channel vars for this call
%% @end
%%-----------------------------------------------------------------------------
-spec get_channel_ccvs/1 :: (#cf_call{}) -> wh_json:json_object().
get_channel_ccvs(#cf_call{channel_vars=CCVs, authorizing_id=AuthId, owner_id=OwnerId
                          ,cid_name=CIDName, cid_number=CIDNumber}=Call) ->
    CCVFuns = [fun(J) ->
                       case OwnerId of
                           undefined -> J;
                           OwnerId ->
                               wh_json:set_value(<<"Owner-ID">>, OwnerId, J)
                       end
                end
               ,fun(J) ->
                        case cf_attributes:moh_attributes(AuthId, <<"media_id">>, Call) of
                            undefined -> J;
                            MediaId ->
                                ?LOG("set hold media to ~s", [MediaId]),
                                wh_json:set_value(<<"Hold-Media">>, MediaId, J)
                        end
                end
               ,fun(J) ->
                        wh_json:set_value(<<"Caller-ID-Name">>, CIDName
                                          ,wh_json:set_value(<<"Caller-ID-Number">>, CIDNumber, J))
                end
               ,fun(J) ->
                        wh_json:set_value(<<"Overwrite-Channel-Vars">>, <<"true">>, J)
                end
%%               ,fun(J) ->
%%                        case AuthId of
%%                            undefined -> J;
%%                            _Else ->
%%                                ?LOG("set transfer fallback to ~s", [AuthId]),
%%                                wh_json:set_value(<<"Transfer-Fallback">>, AuthId, J)
%%                        end
%%                end
              ],
    lists:foldr(fun(F, J) -> F(J) end, CCVs, CCVFuns).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% get the custom call vars for this call
%% @end
%%-----------------------------------------------------------------------------
-spec get_call_ccvs/1 :: (#cf_call{}) -> wh_json:json_object().
get_call_ccvs(_Call) ->
    CCVFuns = [
              ],
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), CCVFuns).
