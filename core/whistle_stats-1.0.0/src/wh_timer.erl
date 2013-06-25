%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%% Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(wh_timer).

-export([new/1
	 ,update/1
	 ,get/1
	 ,get_microseconds/1
	 ,get_milliseconds/1
	 ,get_seconds/1
	 ,get_timestamp/1
	 ,delete/1
	 ,reset/1
	 ,exists/1
	]).

-include_lib("whistle/include/wh_types.hrl").

-define(NAMESPACE, <<"wh_timer">>).
-define(MICROSECONDS, 1).
-define(MILLISECONDS, 1000).
-define(SECONDS, 1000000).

%% ===================================================================
%% API
%% ===================================================================

-spec new/1 :: (ne_binary()) -> true | false.
new(Name) ->
    maybe_init_timer(namespace(Name), erlang:now()).

-spec update/1 :: (ne_binary()) -> ok.
update(Name) ->
    case maybe_init_timer(namespace(Name), erlang:now()) of
	false ->
	    update_timer(namespace(Name), erlang:now());
	true ->
	    ok
    end.

-spec get/1 :: (ne_binary()) -> non_neg_integer().
get(Name) ->
    get_milliseconds(Name).

-spec get_microseconds/1 :: (ne_binary()) -> non_neg_integer().
get_microseconds(Name) ->
    get(namespace(Name), ?MICROSECONDS).

-spec get_milliseconds/1 :: (ne_binary()) -> non_neg_integer().
get_milliseconds(Name) ->
    get(namespace(Name), ?MILLISECONDS).

-spec get_seconds/1 :: (ne_binary()) -> non_neg_integer().
get_seconds(Name) ->
    get(namespace(Name), ?SECONDS).
    
-spec get_timestamp/1 :: (ne_binary()) -> erlang:timestamp().
get_timestamp(Name) ->
    maybe_get_timer(namespace(Name)).

-spec delete/1 :: (ne_binary()) -> true | false.
delete(Name) ->
    maybe_delete_timer(namespace(Name)).

-spec reset/1 :: (ne_binary()) -> true | false.
reset(Name) ->
    maybe_delete_timer(namespace(Name)),
    maybe_init_timer(namespace(Name), erlang:now()).

-spec exists/1 :: (ne_binary()) -> true | false.
exists(Name) ->
    timer_exists(namespace(Name)).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec namespace/1 :: (ne_binary()) -> ne_binary().
namespace(Name) ->
    list_to_binary([?NAMESPACE, <<".">>, Name]).

-spec maybe_init_timer/2 :: (ne_binary(), erlang:timestamp()) -> true | false.
maybe_init_timer(Name, InitialValue) ->
    case timer_exists(Name) of 
	false ->
	    init_timer(Name, InitialValue),
	    true;
	true ->
	    false
    end.

-spec maybe_get_timer/1 :: (ne_binary()) -> erlang:timestamp().
maybe_get_timer(Name) ->
    case timer_exists(Name) of
	true ->
	    get_timer(Name);
	false ->
	    undefined
    end.

-spec maybe_delete_timer/1 :: (ne_binary()) -> true | false.
maybe_delete_timer(Name) ->
    case timer_exists(Name) of
	true ->
	    delete_timer(Name),
	    true;
	false ->
	    false
    end.

-spec get/2 :: (ne_binary(), pos_integer()) -> non_neg_integer().
get(Name, Resolution) ->
    case maybe_get_timer(Name) of
	undefined ->
	    0;
	Timestamp ->
	    timer:now_diff(erlang:now(), Timestamp) div Resolution
    end.
%% ===================================================================
%% Internal functions - Folsom wrapper
%% ===================================================================

-spec init_timer/2 :: (ne_binary(), erlang:timestamp()) -> ok.
init_timer(Name, Value) ->
    %folsom_metrics:new_gauge(Name),
    update_timer(Name, Value).

-spec update_timer/2 :: (ne_binary(), erlang:timestamp()) -> ok.
update_timer(Name, Value) ->
    %folsom_metrics:notify({Name, Value}).
    ok.

-spec get_timer/1 :: (ne_binary()) -> erlang:timestamp().
get_timer(Name) ->
    %folsom_metrics:get_metric_value(Name).
    erlang:now().

-spec delete_timer/1 :: (ne_binary()) -> ok.
delete_timer(Name) ->
    %folsom_metrics:delete_metric(Name).
    ok.

-spec timer_exists/1 :: (ne_binary()) -> true | false.
timer_exists(Name) ->
    %folsom_metrics:metric_exists(Name).
    true.
