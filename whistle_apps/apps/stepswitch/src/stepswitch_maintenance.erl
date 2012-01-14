%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-include("stepswitch.hrl").

%% API
-export([lookup_number/1]).
-export([reload_resources/0]).
-export([process_number/1, process_number/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number/1 :: (string()) -> {ok, binary()} | {error, atom()}.
lookup_number(Number) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {lookup_number, Number}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources/0 :: () -> ok.
reload_resources() ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {reload_resrcs}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of tuples that represent the routing logic for the
%% provided number and flags.  The tuple containts:
%% {Resource ID, Delay (in seconds), SIP URI}
%% @end
%%--------------------------------------------------------------------
-spec process_number/1 :: (string()) -> list() | {error, atom()}.
-spec process_number/2 :: (string(), list()) -> list() | {error, atom()}.

process_number(Number) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {process_number, Number}).

process_number(Number, Flags) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {process_number, Number, Flags}).
