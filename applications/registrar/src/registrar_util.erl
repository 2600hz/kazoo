%% @author root
%% @doc @todo Add description to registrar_util.


-module(registrar_util).

-include("reg.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([bin_to_hexstr/1, hexstr_to_bin/1]).
-export([remove_dashes/1]).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

-spec hexstr_to_bin(binary() | list()) -> binary().
hexstr_to_bin(S) when is_binary(S) ->
    hexstr_to_bin(binary_to_list(S));
hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

-spec remove_dashes(ne_binary()) -> ne_binary().
remove_dashes(Bin) ->
  wh_util:to_binary(lists:filter(fun (B) -> not lists:member(B, "-") end, binary_to_list(Bin))).


%% ====================================================================
%% Internal functions
%% ====================================================================


