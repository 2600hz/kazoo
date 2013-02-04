%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  Tue, 15 Mar 2011 13:42:17 GMT: James Aimonetti <james@2600hz.org>
-module(media_mgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start(StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    case media_mgr:start_link() of
        {ok, P} -> {ok, P};
        {error, {already_started, P} } -> {ok, P};
        {error, _}=E -> E
    end.

stop(_State) ->
    ok.
