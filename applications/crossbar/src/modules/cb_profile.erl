%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_profile).

-export([init/0
        ,req_init/1
        ,req_finish/1
        ]).

-include("crossbar.hrl").

-define(TRACE_PATH, kapps_config:get_ne_binary(?CONFIG_CAT, <<"trace_path">>, <<"/tmp">>)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
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
req_init({Context, _} = InitArgs) ->
    req_init(cb_context:profile_id(Context), InitArgs).

-spec req_init(kz_term:api_binary(), {cb_context:context(), cowboy_req:req()}) ->
                      {cb_context:context(), cowboy_req:req()}.
req_init('undefined', InitArgs) ->
    InitArgs;
req_init(ProfileId, InitArgs) ->
    File = list_to_binary([?TRACE_PATH, ProfileId, ".trace"]),
    case fprof:trace(['start'
                     ,'verbose'
                     ,{'file', kz_term:to_list(File)}
                     ])
    of
        'ok' -> lager:debug("started trace ~s", [File]);
        {'error', _R} ->
            lager:debug("failed to start tracing ~s: ~p", [File, _R]);
        {'EXIT', _SPid, _R} ->
            lager:debug("exception starting tracing ~s in ~p: ~p", [File, _SPid, _R])
    end,
    InitArgs.

-spec req_finish(cb_context:context() | kz_term:api_binary()) -> any().
req_finish('undefined') -> 'ok';
req_finish(ProfileId) when is_binary(ProfileId) ->
    File = list_to_binary([?TRACE_PATH, ProfileId, ".trace"]),
    lager:debug("now run: erlgrind ~s", [File]),
    lager:debug("then run kcachegrind on the .cgrind file created"),
    fprof:trace(['stop'
                ,{'file', kz_term:to_list(File)}
                ]);
req_finish(Context) ->
    req_finish(cb_context:profile_id(Context)).
