%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnip_util).

-export([extract_user/1]).

-include("omnipresence.hrl").

-spec extract_user(ne_binary()) -> {ne_binary(), ne_binary(), ne_binaries()}.
extract_user(<<"sip:", User/binary>>) -> {<<"sip">>, User, binary:split(User, <<"@">>)};
extract_user(User) -> {<<"sip">>, User, binary:split(User, <<"@">>)}.
