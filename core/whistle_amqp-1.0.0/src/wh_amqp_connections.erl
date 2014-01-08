%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_connections).

-export([add/1]).
-export([remove/1]).
-export([is_available/0]).
-export([wait_for_available/0]).

-include("amqp_util.hrl").

-spec add(wh_amqp_connection() | text()) -> wh_amqp_connection() | {'error', _}.
add(#wh_amqp_connection{broker=_Broker}=Connection) ->
    case wh_amqp_connection_sup:add(Connection) of
        {'ok', _} -> Connection;
        {'error', Reason} ->
            lager:warning("unable to start amqp connection to '~s': ~p"
                          ,[_Broker, Reason]),
            {'error', Reason}
    end;
add(Broker) when not is_binary(Broker) ->
    add(wh_util:to_binary(Broker));
add(Broker) ->
    case catch amqp_uri:parse(wh_util:to_list(Broker)) of
        {'EXIT', _R} ->
            lager:error("failed to parse AMQP URI '~s': ~p", [Broker, _R]),
            {'error', 'invalid_uri'};
        {'error', {Info, _}} ->
            lager:error("failed to parse AMQP URI '~s': ~p", [Broker, Info]),
            {'error', 'invalid_uri'};
        {'ok', #amqp_params_network{}=Params} ->
            add(#wh_amqp_connection{broker=Broker
                                    ,params=Params#amqp_params_network{connection_timeout=500}
                                   });
        {'ok', Params} ->
            add(#wh_amqp_connection{broker=Broker
                                    ,params=Params
                                   })
    end.

-spec remove(text()) -> 'ok'.
remove(URI) when not is_binary(URI) ->
    remove(wh_util:to_binary(URI));
remove(URI) -> 'ok'.

-spec current() -> {'ok', ne_binary()} |
                   {'error', 'no_available_connection'}.
current() -> {'ok', 'no_available_connection'}.

-spec is_available() -> boolean().
is_available() ->
    case current() of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec wait_for_available() -> 'ok'.
wait_for_available() ->
    case is_available() of
        'true' -> 'ok';
        'false' ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_available()
    end.
