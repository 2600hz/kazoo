%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%% Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(wh_counter).

-export([new/1
	 ,new/2
	 ,inc/1
	 ,inc/2
	 ,dec/1
	 ,dec/2
	 ,reset/1
	 ,reset/2
	 ,get/1
	 ,delete/1
	 ,exists/1
	]).

-include_lib("whistle/include/wh_types.hrl").

-define(NAMESPACE, <<"wh_counter">>).

%% ===================================================================
%% API
%% ===================================================================

-spec new/1 :: (ne_binary()) -> true | false.
-spec new/2 :: (ne_binary(), integer()) -> true | false.
new(Name) ->
    new(Name, 0).

new(Name, InitialValue) ->
    maybe_init_counter(namespace(Name), InitialValue).

-spec inc/1 :: (ne_binary()) -> ok.
-spec inc/2 :: (ne_binary(), integer()) -> ok.
inc(Name) ->
    inc(Name, 1).

inc(Name, Value) ->
    maybe_init_counter(namespace(Name), 0),
    inc_counter(namespace(Name), Value).

-spec dec/1 :: (ne_binary()) -> ok.
-spec dec/2 :: (ne_binary(), integer()) -> ok.
dec(Name) ->
    dec(Name, 1).

dec(Name, Value) ->
    maybe_init_counter(namespace(Name), 0),
    dec_counter(namespace(Name), Value).

-spec get/1 :: (ne_binary()) -> integer().
get(Name) ->
    maybe_get_counter(namespace(Name)).

-spec delete/1 :: (ne_binary()) -> true | false.
delete(Name) ->
    maybe_delete_counter(namespace(Name)).

-spec reset/1 :: (ne_binary()) -> true | false.
-spec reset/2 :: (ne_binary(), integer()) -> true | false.
reset(Name) ->
    reset(Name, 0).

reset(Name, InitialValue) ->
    maybe_delete_counter(namespace(Name)),
    maybe_init_counter(namespace(Name), InitialValue).

-spec exists/1 :: (ne_binary()) -> true | false. 
exists(Name) ->
    counter_exists(namespace(Name)).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec namespace/1 :: (ne_binary()) -> ne_binary().
namespace(Name) ->
    list_to_binary([?NAMESPACE, <<".">>, Name]).

-spec maybe_init_counter/2 :: (ne_binary(), integer()) -> true | false.
maybe_init_counter(Name, InitialValue) ->
    case counter_exists(Name) of
	false ->
	    init_counter(Name, InitialValue),
	    true;
	true ->
	    false
    end.

-spec maybe_get_counter/1 :: (ne_binary()) -> integer().
maybe_get_counter(Name) ->
    case counter_exists(Name) of
	true ->
	    get_counter(Name);
	false ->
	    0
    end.

-spec maybe_delete_counter/1 :: (ne_binary()) -> true | false.
maybe_delete_counter(Name) ->
    case counter_exists(Name) of
	true ->
	    delete_counter(Name),
	    true;
	false ->
	    false
    end.

%% ===================================================================
%% Internal functions - Folsom wrapper
%% ===================================================================

-spec init_counter/2 :: (ne_binary(), integer()) -> ok.
init_counter(Name, Value) when Value > 0 ->
    %folsom_metrics:new_counter(Name),
    inc_counter(Name, Value);
init_counter(Name, Value) when Value < 0 ->
    %folsom_metrics:new_counter(Name),
    dec_counter(Name, Value * -1);
init_counter(Name, 0) ->
    %folsom_metrics:new_counter(Name).
    ok.

-spec inc_counter/2 :: (ne_binary(), integer()) -> ok.
inc_counter(Name, Value) ->
    %folsom_metrics:notify({Name, {inc, Value}}).
    0.

-spec dec_counter/2 :: (ne_binary(), integer()) -> ok.
dec_counter(Name, Value) ->
    %folsom_metrics:notify({Name, {dec, Value}}).
    0.

-spec get_counter/1 :: (ne_binary()) -> integer().
get_counter(Name) ->
    %folsom_metrics:get_metric_value(Name).
    0.

-spec delete_counter/1 :: (ne_binary()) -> ok.
delete_counter(Name) ->
    %folsom_metrics:delete_metric(Name).
    ok.

-spec counter_exists/1 :: (ne_binary()) -> true | false.
counter_exists(Name) ->
    %folsom_metrics:metric_exists(Name).
    true.
