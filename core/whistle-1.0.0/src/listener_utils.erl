%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Utility functions for AMQP listeners to use to add/remove responders
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(listener_utils).

-export([add_responder/3
         ,rm_responder/3
        ]).

-define(DEFAULT_CALLBACK, 'handle_req').

%% { {Event-Category, Event-Name}, CallbackModule | {CallbackModule, Function} }
-type responder_callback() :: {atom(), atom()}.
-type responder() :: {{binary(), binary()}, responder_callback()}.
-type responders() :: [responder(),...] | [].
-export_type([responder/0
              ,responders/0
             ]).

-spec add_responder(responders(), responder_callback(), wh_json:json_proplist()) -> responders().
add_responder(Responders, Responder, Keys) when is_atom(Responder) ->
    add_responder(Responders, {Responder, ?DEFAULT_CALLBACK}, Keys);
add_responder(Responders, Responder, Keys) ->
    _ = maybe_init_responder(Responder, is_responder_known(Responders, Responder)),
    lists:foldr(fun maybe_add_mapping/2, Responders, [{Evt, Responder} || Evt <- Keys]).

-spec rm_responder(responders(), responder_callback(), wh_json:json_proplist()) -> responders().
%% remove all events for responder
rm_responder(Responders, Responder, Keys) when is_atom(Responder) ->
    rm_responder(Responders, {Responder, ?DEFAULT_CALLBACK}, Keys);
rm_responder(Responders, Responder, []) ->
    [ N || {_, Module}=N <- Responders, Module =/= Responder];
%% remove events in Keys for module Responder
rm_responder(Responders, Responder, Keys) ->
    %% if Evt is in the list of Keys and Module =:= Responder, remove it from the list of Responders
    [ N || {Evt, Module}=N <- Responders, (not (Module =:= Responder andalso lists:member(Evt, Keys))) ].

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec is_responder_known(responders(), responder_callback()) -> boolean().
is_responder_known(Responders, {Responder,_}=Callback) ->
    _ = maybe_load_responder(Responder),
    erlang:function_exported(Responder, 'init', 0) andalso
        wh_util:is_false(lists:keyfind(Callback, 2, Responders)).

maybe_load_responder(Responder) ->
    case erlang:module_loaded(Responder) of
        'true' -> 'ok';
        'false' -> {'module', Responder} = code:ensure_loaded(Responder)
    end.

-spec maybe_add_mapping(responder(), responders()) -> responders().
maybe_add_mapping(Mapping, Acc) ->
    case lists:member(Mapping, Acc) of
        'true' -> Acc;
        'false' -> [Mapping | Acc]
    end.

-spec maybe_init_responder(responder_callback(), boolean()) -> 'ok'.
maybe_init_responder({Responder, _Fun}, 'true') when is_atom(Responder) ->
    catch Responder:init(), 'ok';
maybe_init_responder(_,_) ->
    'ok'.
