%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnip_util).

-export([extract_user/1
         ,normalize_variables/1
        ]).

-include("omnipresence.hrl").
-include_lib("nksip/include/nksip.hrl").

-spec extract_user(ne_binary()) -> {ne_binary(), ne_binary(), ne_binaries()}.
extract_user(User) ->
    [#uri{scheme=Proto, user=Username, domain=Realm}] = nksip_parse:uris(User),
    {wh_util:to_binary(Proto), <<Username/binary, "@", Realm/binary>>, [Username, Realm]}.

-spec normalize_variables(wh_proplist()) -> wh_proplist().
normalize_variables(Props) ->
    [{wh_json:normalize_key(K), V} || {K, V} <- Props].
