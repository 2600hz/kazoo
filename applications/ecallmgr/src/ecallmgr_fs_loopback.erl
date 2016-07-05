%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Filter loopback properties
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_loopback).



-export([filter/3, filter/4]).

-include("ecallmgr.hrl").

-define(IS_LOOPBACK(Props), props:get_value(<<"variable_loopback_leg">>, Props) =:= <<"B">>
            andalso props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, Props) =:= 'undefined'
        andalso props:get_value(<<?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, Props) =:= 'undefined'
       ).
-define(LOOPBACK_FILTERED, "Filtered-By-Loopback").


-spec filter(atom(), ne_binary(), kz_proplist()) -> kz_proplist().
filter(Node, UUID, Props) ->
    filter(?IS_LOOPBACK(Props), Node, UUID, Props, 'false').

-spec filter(atom(), ne_binary(), kz_proplist(), boolean()) -> kz_proplist().
filter(Node, UUID, Props, Update) ->
    filter(?IS_LOOPBACK(Props), Node, UUID, Props, Update).

-spec filter(boolean(), atom(), ne_binary(), kz_proplist(), boolean()) -> kz_proplist().
filter('false', _Node, _UUID, Props, _) -> Props;
filter('true', Node, UUID, Props, Update) ->
    case lists:foldl(fun filter_loopback/2, [], Props) of
        [] -> Props;
        LoopBackCCVs ->
            UpdateCCVs = [{<<?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, <<"true">>} | LoopBackCCVs],
            {Clear, Filtered} = lists:foldl(fun filter_props/2, {[], []}, Props),
            Funs = fun() ->
                           ecallmgr_fs_command:unset(Node, UUID, Clear),
                           ecallmgr_fs_command:set(Node, UUID, UpdateCCVs)
                   end,
            maybe_update_ccvs(Update, Funs, updated_props(Filtered, UpdateCCVs))
    end.

-spec updated_props(kz_proplist(), kz_proplist()) -> kz_proplist().
updated_props(Filtered, New) ->
    Filtered ++ New ++ [ {<<"variable_", K/binary>>, V} || {K, V} <- New].

-spec maybe_update_ccvs(boolean(), fun(), kz_proplist()) -> kz_proplist().
maybe_update_ccvs('false', _Fun, Filtered) -> Filtered;
maybe_update_ccvs('true', Fun, Filtered) ->
    kz_util:spawn(Fun),
    Filtered.

-spec filter_loopback({ne_binary(), any()}, kz_proplist()) -> kz_proplist().
filter_loopback({<<?CHANNEL_VAR_PREFIX, ?CHANNEL_LOOPBACK_HEADER_PREFIX, K/binary>>, V}, Acc) ->
    [{<<?CHANNEL_VAR_PREFIX, K/binary>>, V} | Acc];
filter_loopback({<<"variable_", ?CHANNEL_VAR_PREFIX, ?CHANNEL_LOOPBACK_HEADER_PREFIX, K/binary>>, V}, Acc) ->
    [{<<?CHANNEL_VAR_PREFIX, K/binary>>, V} | Acc];
filter_loopback(_, Acc) -> Acc.

-spec filter_props({ne_binary(), any()}, {kz_proplist(), kz_proplist()}) -> {kz_proplist(), kz_proplist()}.
filter_props({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, _V}, {Clear, Filtered}) ->
    {[<<?CHANNEL_VAR_PREFIX, Key/binary>> | Clear], Filtered};
filter_props({<<?CHANNEL_VAR_PREFIX, Key/binary>>, _V}, {Clear, Filtered}) ->
    {[<<?CHANNEL_VAR_PREFIX, Key/binary>> | Clear], Filtered};
filter_props({<<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>, _V}, {Clear, Filtered}) ->
    {[<<"sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>> | Clear], Filtered};
filter_props(KV, {Clear, Filtered}) ->
    {Clear, [KV | Filtered]}.
