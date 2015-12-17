%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(whistle_transactions_maintenance).

-export([disable_top_up/0]).
-export([enable_top_up/0]).

-include("../include/whistle_transactions.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec enable_top_up() -> 'ok'.
enable_top_up() ->
    case whapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'true') of
        {'ok', _} -> io:format("top up enabled ~n", []);
        {'error', _E} -> io:format("top up failed to enable : ~p~n", [_E])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec disable_top_up() -> 'ok'.
disable_top_up() ->
    case whapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'false') of
        {'ok', _} -> io:format("top up disabled ~n", []);
        {'error', _E} -> io:format("top up failed to disable : ~p~n", [_E])
    end.
