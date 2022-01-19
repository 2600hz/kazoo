%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(conference_maintenance).

-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).
-export([flush/0]).

-include("conference.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @deprecated This function is deprecated, please use
%% {@link kapps_maintenance:refresh/0} instead.
%% @end
%%------------------------------------------------------------------------------
-spec blocking_refresh() -> 'ok'.
blocking_refresh() ->
    io:format("This function is deprecated, please use kapps_maintenance:refresh() instead.").

%%------------------------------------------------------------------------------
%% @doc
%% @deprecated This function is deprecated, please use
%% {@link kapps_maintenance:refresh/0} instead.
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    io:format("This function is deprecated, please use kapps_maintenance:refresh() instead.").

%%------------------------------------------------------------------------------
%% @doc
%% @deprecated This function is deprecated, please use
%% {@link kapps_maintenance:refresh/1} instead.
%% @end
%%------------------------------------------------------------------------------
-spec refresh(any()) -> 'ok'.
refresh(_Account) ->
    io:format("This function is deprecated, please use kapps_maintenance:refresh(~p) instead.", [_Account]).

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).
