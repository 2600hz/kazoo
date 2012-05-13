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

    lager:debug("received route win"),
    case whapps_call:retrieve(CallId) of
        {ok, Call} ->
            lager:debug("bootstrapping callflow executer"),
            Updaters = [fun(C) -> 
                                CCVUpdaters = get_channel_ccvs_updaters(C),
                                whapps_call:update_custom_channel_vars(CCVUpdaters, C)
                        end
                        ,fun(C) -> 
                                 {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, C),
                                 whapps_call:set_caller_id_name(CIDName, whapps_call:set_caller_id_number(CIDNumber, C)) 
                         end
                        ,fun(C) -> 
                                 OwnerId = cf_attributes:owner_id(C),
                                 whapps_call:kvs_store(owner_id, OwnerId, C)
                         end
                        ,fun(C) -> whapps_call:from_route_win(JObj, C) end
                       ],
            execute_callflow(lists:foldr(fun(F, C) -> F(C) end, Call, Updaters));
        {error, R} ->
            lager:debug("unable to find callflow during second lookup (HUH?) ~p", [R])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% executes the found call flow by starting a new cf_exe process under the
%% cf_exe_sup tree.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_callflow/1 :: (whapps_call:call()) -> {'ok', pid()}.
execute_callflow(Call) ->
    lager:debug("call has been setup, passing control to callflow executer"),
    cf_exe_sup:new(Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% get the custom channel vars for this call
%% @end
%%-----------------------------------------------------------------------------
-spec get_channel_ccvs_updaters/1 :: (whapps_call:call()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
get_channel_ccvs_updaters(Call) ->
    [fun(J) ->
              case cf_attributes:moh_attributes(<<"media_id">>, Call) of
                  undefined -> J;
                  MediaId ->
                      lager:debug("set hold media to ~s", [MediaId]),
                      wh_json:set_value(<<"Hold-Media">>, MediaId, J)
              end
      end
     ,fun(J) ->
              wh_json:set_value(<<"Overwrite-Channel-Vars">>, <<"true">>, J)
      end
    ].
