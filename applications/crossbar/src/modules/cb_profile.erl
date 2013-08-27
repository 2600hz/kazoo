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

-define(CB_LIST, <<"skels/crossbar_listing">>).

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

    _ = crossbar_bindings:bind(<<"v1_resource.init">>, ?MODULE, 'req_init'),
    _ = crossbar_bindings:bind(<<"v1_resource.finish_request.*.*">>, ?MODULE, 'req_finish').

req_init({#cb_context{profile_id='undefined'}, _}=InitArgs) ->
    InitArgs;
req_init({#cb_context{profile_id=ProfileId}, _}=InitArgs) ->
    File = list_to_binary(["/tmp/", ProfileId, ".trace"]),
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

req_finish(#cb_context{profile_id='undefined'}) -> 'ok';
req_finish(#cb_context{profile_id=ProfileId}) ->
    File = list_to_binary(["/tmp/", ProfileId, ".trace"]),
    lager:debug("now run: erlgrind ~s", [File]),
    lager:debug("then run kcachegrind on the .cgrind file created"),
    fprof:trace(['stop'
                 ,{'file', wh_util:to_list(File)}
                ]).
