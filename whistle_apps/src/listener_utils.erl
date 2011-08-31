%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Utility functions for AMQP listeners to use to add/remove responders
%%% @end
%%% Created : 17 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(listener_utils).

-export([add_responder/3, rm_responder/3]).

-type responder() :: {{binary(), binary()}, atom()}.
-export_type([responder/0]).

-spec add_responder/3 :: (Responders, Responder, Keys) -> [responder(),...] when
      Responders :: [responder(),...] | [],
      Responder :: atom(),
      Keys :: [{binary(), binary()},...].
add_responder(Responders, Responder, Keys) ->
    _ = maybe_init_responder(Responder, is_responder_known(Responders, Responder)),
    lists:foldr(fun maybe_add_mapping/2, Responders, [{Evt, Responder} || Evt <- Keys]).

-spec rm_responder/3 :: (Responders, Responder, Keys) -> [responder(),...] when
      Responders :: [responder(),...] | [],
      Responder :: atom(),
      Keys :: [{binary(), binary()},...] | [].
%% remove all events for responder
rm_responder(Responders, Responder, []) ->
    [ N || {_, Module}=N <- Responders, Module =/= Responder];
%% remove events in Keys for module Responder
rm_responder(Responders, Responder, Keys) ->
    %% if Evt is in the list of Keys and Module =:= Responder, remove it from the list of Responders
    [ N || {Evt, Module}=N <- Responders, (not (Module =:= Responder andalso lists:member(Evt, Keys))) ].

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec is_responder_known/2 :: (Responders, Responder) -> boolean() when
      Responders :: [responder(),...] | [],
      Responder :: atom().
is_responder_known(Responders, Responder) ->
    _ = maybe_load_responder(Responder),
    erlang:function_exported(Responder, init, 0) andalso wh_util:is_false(lists:keyfind(Responder, 2, Responders)).

maybe_load_responder(Responder) ->
    case erlang:module_loaded(Responder) of
	true -> ok;
	false -> {module, Responder} = code:ensure_loaded(Responder)
    end.

-spec maybe_add_mapping/2 :: (Mapping, Acc) -> [responder(),...] when
      Mapping :: responder(),
      Acc :: [responder(),...] | [].
maybe_add_mapping(Mapping, Acc) ->
    case lists:member(Mapping, Acc) of
	true -> Acc;
	false -> [Mapping | Acc]
    end.

-spec maybe_init_responder/2 :: (Responder, DoInit) -> ok when
      Responder :: atom(),
      DoInit :: boolean().
maybe_init_responder(Responder, true) when is_atom(Responder) ->
    catch(Responder:init()), ok;
maybe_init_responder(_,_) ->
    ok.
