%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(media_mgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("whistle/include/wh_types.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_StartType, _StartArgs) ->
    case media_mgr:start_link() of
        {'ok', P} -> {'ok', P};
        {'error', {'already_started', P} } -> {'ok', P};
        {'error', _}=E -> E
    end.

stop(_State) -> media_mgr:stop(), 'ok'.
