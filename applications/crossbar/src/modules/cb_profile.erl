%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_profile).

-export([init/0
         ,req_init/1
         ,req_finish/1
        ]).

-include("../crossbar.hrl").

-define(TRACE_PATH, whapps_config:get(?CONFIG_CAT, <<"trace_path">>, <<"/tmp">>)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    case fprof:start() of
        {'ok', _P} -> lager:debug("fprof pid: ~p", [_P]);
        {'error', {'already_started', _P}} -> lager:debug("fprof pid: ~p", [_P]);
        {'error', _R} -> lager:debug("failed to start fprof: ~p", [_R])
    end,

    _ = crossbar_bindings:bind(<<"*.init">>, ?MODULE, 'req_init'),
    _ = crossbar_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'req_finish').

-spec req_init({cb_context:context(), cowboy_req:req()}) ->
                      {cb_context:context(), cowboy_req:req()}.
req_init({#cb_context{profile_id='undefined'}, _}=InitArgs) ->
    InitArgs;
req_init({#cb_context{profile_id=ProfileId}, _}=InitArgs) ->
    File = list_to_binary([?TRACE_PATH, ProfileId, ".trace"]),
    case fprof:trace(['start'
                      ,'verbose'
                      ,{'file', wh_util:to_list(File)}
                     ])
    of
        'ok' -> lager:debug("started trace ~s", [File]);
        {'error', _R} ->
            lager:debug("failed to start tracing ~s: ~p", [File, _R]);
        {'EXIT', _SPid, _R} ->
            lager:debug("exception starting tracing ~s in ~p: ~p", [File, _SPid, _R])
    end,
    InitArgs.

-spec req_finish(cb_context:context()) -> any().
req_finish(#cb_context{profile_id='undefined'}) -> 'ok';
req_finish(#cb_context{profile_id=ProfileId}) ->
    File = list_to_binary([?TRACE_PATH, ProfileId, ".trace"]),
    lager:debug("now run: erlgrind ~s", [File]),
    lager:debug("then run kcachegrind on the .cgrind file created"),
    fprof:trace(['stop'
                 ,{'file', wh_util:to_list(File)}
                ]).
