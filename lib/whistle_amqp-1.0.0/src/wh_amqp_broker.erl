%%%============================================================================
%%% @copyright (C) 2011-2012 VoIP Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%============================================================================
-module(wh_amqp_broker).

-include("amqp_util.hrl").

-export([new/0]).
-export([name/1]).
-export([uri/1, set_uri/2]).
-export([use_federation/1, set_use_federation/2]).
-export([params/1]).
-export([is_available/1, set_is_available/2]).

-record(amqp_broker, {uri :: 'undefined' | string()
                      ,params :: 'undefined' | #'amqp_params_direct'{} | #'amqp_params_network'{}
                      ,use_federation = false :: boolean()
                      ,is_available = false :: boolean()
                     }).

-opaque broker() :: #amqp_broker{}.
-export_type([broker/0]).

-spec new/0 :: () -> broker().
new() ->
    #amqp_broker{}.

-spec name/1 :: (broker()) -> 'undefined' | atom().
name(#amqp_broker{uri=URI}) ->
    wh_util:to_atom(URI, true).

-spec uri/1 :: (broker()) -> 'undefined' | atom().
uri(#amqp_broker{uri=URI}) ->    
    URI.

-spec set_uri/2 :: (atom() | string() | ne_binary(), broker()) -> broker().
set_uri(URI, Broker) ->
    U = wh_util:to_list(URI),
    {ok, Params} = amqp_uri:parse(U),
    Broker#amqp_broker{uri=U, params=Params}.

-spec use_federation/1 :: (broker()) -> boolean().
use_federation(#amqp_broker{use_federation=UseFederation}) ->    
    UseFederation.

-spec set_use_federation/2 :: (atom() | string() | ne_binary(), broker()) -> broker().
set_use_federation(UseFederation, Broker) ->
    Broker#amqp_broker{use_federation=wh_util:is_true(UseFederation)}.

-spec params/1 :: (broker()) -> 'undefined' | #'amqp_params_direct'{} | #'amqp_params_network'{}.
params(#amqp_broker{params=Params}) ->    
    Params.
            
-spec is_available/1 :: (broker()) -> boolean().
is_available(#amqp_broker{is_available=IsAvailable}) ->    
    IsAvailable.

-spec set_is_available/2 :: (atom() | string() | ne_binary(), broker()) -> broker().
set_is_available(IsAvailable, Broker) ->
    Broker#amqp_broker{is_available=wh_util:is_true(IsAvailable)}.
