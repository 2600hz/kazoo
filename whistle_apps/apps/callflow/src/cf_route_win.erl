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

-spec handle_req/2 :: (json_object(), proplist()) -> no_return().
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
            C = Call#cf_call{ctrl_q = wh_json:get_value(<<"Control-Queue">>, JObj)
                             ,capture_group = wh_json:get_value(<<"capture_group">>, Flow)
                             ,owner_id = OwnerId
                             ,cid_name = CIDName
                             ,cid_number = CIDNumber
                            },
            execute_call_flow(Flow, C);
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
-spec execute_call_flow/2 :: (json_object(), #cf_call{}) -> no_return().
execute_call_flow(Flow, Call) ->
    cf_call_command:set(get_channel_ccvs(Call), get_call_ccvs(Call), Call),
    ?LOG("call has been setup, passing control to callflow executer"),
    cf_exe_sup:start_proc(Call, wh_json:get_value(<<"flow">>, Flow)).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% get the custom channel vars for this call
%% @end
%%-----------------------------------------------------------------------------
-spec get_channel_ccvs/1 :: (#cf_call{}) -> json_object().
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
-spec get_call_ccvs/1 :: (#cf_call{}) -> json_object().
get_call_ccvs(_Call) ->
    CCVFuns = [
              ],
    lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, CCVFuns).
