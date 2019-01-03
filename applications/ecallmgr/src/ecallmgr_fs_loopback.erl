%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Filter loopback properties
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_loopback).

-export([filter/3, filter/4]).

-include("ecallmgr.hrl").

-define(IS_LOOPBACK(Props), props:get_value(<<"variable_loopback_leg">>, Props) =:= <<"B">>
            andalso props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, Props) =:= 'undefined'
        andalso props:get_value(<<?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, Props) =:= 'undefined'
       ).
-define(LOOPBACK_FILTERED, "Filtered-By-Loopback").

-spec filter(atom(), kz_term:ne_binary(), kzd_freeswitch:data()) -> kz_term:proplist().
filter(Node, UUID, Props) ->
    filter(?IS_LOOPBACK(Props), Node, UUID, Props, 'false').

-spec filter(atom(), kz_term:ne_binary(), kzd_freeswitch:data(), boolean()) -> kz_term:proplist().
filter(Node, UUID, Props, Update) ->
    filter(?IS_LOOPBACK(Props), Node, UUID, Props, Update).

-spec filter(boolean(), atom(), kz_term:ne_binary(), kzd_freeswitch:data(), boolean()) -> kz_term:proplist().
filter('false', _Node, _UUID, Props, _) -> Props;
filter('true', Node, UUID, Props, Update) ->
    case lists:foldl(fun filter_loopback/2, [], Props) of
        [] -> Props;
        LoopBackCCVs ->
            UpdateCCVs = [{<<?CHANNEL_VAR_PREFIX, ?LOOPBACK_FILTERED>>, <<"true">>} | LoopBackCCVs],
            {Clear, Filtered} = lists:foldl(fun filter_props/2, {[], []}, Props),
            Funs = fun() ->
                           _ = ecallmgr_fs_command:unset(Node, UUID, Clear),
                           ecallmgr_fs_command:set(Node, UUID, UpdateCCVs)
                   end,
            maybe_update_ccvs(Update, Funs, updated_props(Filtered, UpdateCCVs))
    end.

-spec updated_props(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
updated_props(Filtered, New0) ->
    New = [{ecallmgr_util:get_fs_key(K), V} || {K, V} <- New0],
    New ++ [{<<"variable_", K/binary>>, V} || {K, V} <- New] ++ Filtered.

-spec maybe_update_ccvs(boolean(), fun(), kz_term:proplist()) -> kz_term:proplist().
maybe_update_ccvs('false', _Fun, Filtered) -> Filtered;
maybe_update_ccvs('true', Fun, Filtered) ->
    kz_util:spawn(Fun),
    Filtered.

-spec filter_loopback({kz_term:ne_binary(), any()}, kz_term:proplist()) -> kz_term:proplist().
filter_loopback({<<?CHANNEL_VAR_PREFIX, ?CHANNEL_LOOPBACK_HEADER_PREFIX, K/binary>>, V}, Acc) ->
    [{<<?CHANNEL_VAR_PREFIX, K/binary>>, V} | Acc];
filter_loopback({<<"variable_", ?CHANNEL_VAR_PREFIX, ?CHANNEL_LOOPBACK_HEADER_PREFIX, K/binary>>, V}, Acc) ->
    [{<<?CHANNEL_VAR_PREFIX, K/binary>>, V} | Acc];
filter_loopback(_, Acc) -> Acc.

-spec filter_props({kz_term:ne_binary(), any()}, {kz_term:proplist(), kz_term:proplist()}) -> {kz_term:proplist(), kz_term:proplist()}.
filter_props({?GET_CCV(Key), _V}, {Clear, Filtered}) ->
    {[?CCV(Key) | Clear], Filtered};
filter_props({?CCV(_)=Key, _V}, {Clear, Filtered}) ->
    {[Key | Clear], Filtered};
filter_props({?GET_CCV_HEADER(Key), _V}, {Clear, Filtered}) ->
    {[<<"sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>> | Clear], Filtered};
filter_props(KV, {Clear, Filtered}) ->
    {Clear, [KV | Filtered]}.
