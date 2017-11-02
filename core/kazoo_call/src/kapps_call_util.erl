%%============================================================================
%%% @copyright (C) 2011-2017 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%============================================================================
-module(kapps_call_util).

-include("kapps_call_command.hrl").

-export([filter_ccvs/1]).

-spec filter_ccvs(kz_json:object()) -> kz_proplist().
filter_ccvs(CCVs) ->
    lager:debug("extracting CCVs from ~p", [CCVs]),
    {ReqCCVs, _} =
        kz_json:foldl(fun filter_ccvs/3
                     ,{[], reserved_ccv_keys()}
                     ,CCVs
                     ),
    ReqCCVs.

filter_ccvs(Key, Value, {Acc, Keys}) ->
    case is_private_ccv(Key, Keys) of
        'true' -> {Acc, Keys};
        'false' ->
            lager:debug("adding ccv ~s:~p", [Key, Value]),
            {[{Key, Value} | Acc], Keys}
    end.

-spec is_private_ccv(ne_binary(), ne_binaries()) -> boolean().
is_private_ccv(Key, Keys) ->
    lists:member(Key, Keys).

-spec reserved_ccv_keys() -> ne_binaries().
reserved_ccv_keys() ->
    kapps_config:get_ne_binaries(<<"call_command">>, <<"reserved_ccv_keys">>, ?DEFAULT_CCV_KEYS).
