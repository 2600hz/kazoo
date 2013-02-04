%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_app).

-behaviour(application).

-include("acdc.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(term(), term()) ->
                         {'ok', pid()} |
                         {'error', startlink_err()}.
start(_StartType, _StartArgs) ->
    case acdc_sup:start_link() of
        {ok, P} -> {ok, P};
        {error, {already_started, P} } -> {ok, P};
        {error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) ->
    ok.
