%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Standard interface for client modules to use to get dialplan commands
%%% translated into 2600Hz-specific commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wht_translator).

-export([exec/2, exec/3]).

exec(Call, Cmds) ->
    exec(Call, Cmds, <<"text/xml">>).

exec(Call, Cmds, CT) when not is_binary(CT) ->
    exec(Call, Cmds, wh_util:to_binary(CT));
exec(Call, Cmds, CT) ->
    case find_candidate_translators(CT) of
        [] -> throw({error, no_translators, CT});
        Mods ->
            case [M || M <- Mods, is_recognized(M, Cmds)] of
                [] -> throw({error, unrecognized_cmds});
                [Translator|_] ->
                    Translator:exec(Call, Cmds)
            end
    end.

find_candidate_translators(<<"text/xml">>) ->
    [wht_twiml];
find_candidate_translators(<<"application/xml">>) ->
    [wht_twiml];
find_candidate_translators(<<"application/json">>) ->
    [wht_2600hz];
find_candidate_translators(_) ->
    [].

is_recognized(M, Cmds) ->
    case catch M:does_recognize(Cmds) of
        true -> true;
        _ -> false
    end.
