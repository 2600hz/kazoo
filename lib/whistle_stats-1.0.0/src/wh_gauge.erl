%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%% Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(wh_gauge).

-export([new/1
	 ,new/2
	 ,set/2
	 ,get/1
	 ,delete/1
	 ,reset/1
	 ,reset/2
	 ,exists/1
	]).

-include_lib("whistle/include/wh_types.hrl").

-define(NAMESPACE, <<"wh_gauge">>).

-type gauge() :: term().

%% ===================================================================
%% API
%% ===================================================================

-spec new/1 :: (ne_binary()) -> true | false.
-spec new/2 :: (ne_binary(), gauge()) -> true | false.
new(Name) ->
    new(Name, 0).

new(Name, InitialValue) ->
    maybe_init_gauge(namespace(Name), InitialValue).

-spec set/2 :: (ne_binary(), gauge()) -> ok.
set(Name, Value) ->
    maybe_init_gauge(namespace(Name), 0),
    set_gauge(namespace(Name), Value).

-spec get/1 :: (ne_binary()) -> gauge().
get(Name) ->
    maybe_get_gauge(namespace(Name)).

-spec delete/1 :: (ne_binary()) -> true | false.
delete(Name) ->
    maybe_delete_gauge(namespace(Name)).

-spec reset/1 :: (ne_binary()) -> true | false.
-spec reset/2 :: (ne_binary(), gauge()) -> true | false.
reset(Name) ->
    reset(Name, 0).

reset(Name, InitialValue) ->
    maybe_delete_gauge(namespace(Name)),
    maybe_init_gauge(namespace(Name), InitialValue).

-spec exists/1 :: (ne_binary()) -> true | false.
exists(Name) ->
    gauge_exists(namespace(Name)).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec namespace/1 :: (ne_binary()) -> ne_binary().
namespace(Name) ->
    list_to_binary([?NAMESPACE, <<".">>, Name]).

-spec maybe_init_gauge/2 :: (ne_binary(), gauge()) -> true | false.
maybe_init_gauge(Name, InitialValue) ->
    case gauge_exists(Name) of 
	false ->
	    init_gauge(Name, InitialValue),
	    true;
	true ->
	    false
    end.

-spec maybe_get_gauge/1 :: (ne_binary()) -> gauge().
maybe_get_gauge(Name) ->
    case gauge_exists(Name) of
	true ->
	    get_gauge(Name);
	false ->
	    undefined
    end.

-spec maybe_delete_gauge/1 :: (ne_binary()) -> true | false.
maybe_delete_gauge(Name) ->
    case gauge_exists(Name) of
	true ->
	    delete_gauge(Name),
	    true;
	false ->
	    false
    end.

%% ===================================================================
%% Internal functions - Folsom wrapper
%% ===================================================================

-spec init_gauge/2 :: (ne_binary(), gauge()) -> ok.
init_gauge(Name, Value) ->
    %folsom_metrics:new_gauge(Name),
    set_gauge(Name, Value).

-spec set_gauge/2 :: (ne_binary(), gauge()) -> ok.
set_gauge(Name, Value) ->
    %folsom_metrics:notify({Name, Value}).
    ok.

-spec get_gauge/1 :: (ne_binary()) -> gauge().
get_gauge(Name) ->
    %folsom_metrics:get_metric_value(Name).
    0.

-spec delete_gauge/1 :: (ne_binary()) -> ok.
delete_gauge(Name) ->
    %folsom_metrics:delete_metric(Name).
    ok.

-spec gauge_exists/1 :: (ne_binary()) -> true | false.
gauge_exists(Name) ->
    %folsom_metrics:metric_exists(Name).
    true.
