%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hangups_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) -> {'ok', pid()} | {'error', startlink_err()}.
start(_, _) ->
    case hangups:start_link() of
        {'ok', P} -> {'ok', P};
        {'error', {'already_started', P}} -> {'ok', P};
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_) ->
    'ok'.
