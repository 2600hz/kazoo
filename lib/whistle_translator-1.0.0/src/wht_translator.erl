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

-export([exec/2, exec/3
         ,get_user_vars/1
         ,set_user_vars/2
        ]).

-include("wht.hrl").

-spec exec/2 :: (whapps_call:call(), list()) -> exec_return().
-spec exec/3 :: (whapps_call:call(), list(), ne_binary() | list()) -> exec_return().
exec(Call, Cmds) ->
    exec(Call, Cmds, <<"text/xml">>).

exec(Call, Cmds, CT) when not is_binary(CT) ->
    exec(Call, Cmds, wh_util:to_binary(CT));
exec(Call, Cmds, CT) ->
    case find_candidate_translators(CT) of
        [] -> throw({error, no_translators, CT});
        Mods ->
            case [{M, Cmd1} || M <- Mods,
                               begin {IsRecognized, Cmd1} = is_recognized(M, Cmds), IsRecognized end
                 ] of
                [] -> throw({error, unrecognized_cmds});
                [{Translator, Cmds1}|_] ->
                    Translator:exec(Call, Cmds1)
            end
    end.

-spec get_user_vars/1 :: (whapps_call:call()) -> wh_json:json_object().
-spec set_user_vars/2 :: (proplist(), whapps_call:call()) -> wh_json:json_object().
get_user_vars(Call) ->
    whapps_call:kvs_fetch(?WHT_USER_VARS, wh_json:new(), Call).

set_user_vars(Prop, Call) ->
    UserVars = get_user_vars(Call),
    whapps_call:kvs_store(?WHT_USER_VARS, wh_json:set_values(Prop, UserVars), Call).

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
        {true, _}=True -> True;
        _F -> {false, []}
    end.
