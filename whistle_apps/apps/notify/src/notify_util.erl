%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 Oct 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_util).

-export([filter_subscribers/2, filter_subscriber/2, lookup_subscribers/2]).

-include("notify.hrl").
-include_lib("whistle/include/wh_types.hrl").

filter_subscribers(Subscribers, Fields) ->
    [filter_subscriber(Subscriber, Fields)
     || Subscriber <- Subscribers].

filter_subscriber(Subscriber, Fields) ->
    wh_json:from_list(lists:foldl(fun(F, Acc) ->
                                          case wh_json:get_value(F, Subscriber) of
                                              undefined -> Acc;
                                              V -> [{F, V}|Acc]
                                          end
                                  end, [], Fields)).

lookup_subscribers(User, Realm) ->
    {ok, Cache} = notify_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, {notify_presentity, User, Realm}) of
        {error, not_found} ->
            [];
        {ok, Watchers} ->
            {Subscribers, NewWatchers} =
                lists:foldr(fun({U, R}, {SAcc, WAcc}=Acc) ->
                                case wh_cache:peek_local(Cache, {notify_watcher, U, R}) of
                                    {ok, Subscriber} ->
                                        {[Subscriber|SAcc], [{U, R}|WAcc]};
                                    {error, not_found} ->
                                        ?LOG("removing ~s@~s from watchers for ~s@~s", [U, R, User, Realm]),
                                        Acc
                                end
                        end, {[], []}, Watchers),
            wh_cache:store_local(Cache, {notify_presentity, User, Realm}, NewWatchers),
            Subscribers
    end.
