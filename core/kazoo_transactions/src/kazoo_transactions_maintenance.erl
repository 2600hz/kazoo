%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_transactions_maintenance).

-export([disable_top_up/0]).
-export([enable_top_up/0]).

-include("include/kazoo_transactions.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec enable_top_up() -> 'ok'.
enable_top_up() ->
    kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'true'),
    io:format("top up enabled ~n").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec disable_top_up() -> 'ok'.
disable_top_up() ->
    kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'false'),
    io:format("top up disabled ~n", []).
