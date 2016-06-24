%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Standard interface for client modules to use to get dialplan commands
%%% translated into 2600Hz-specific commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_translator).

-export([exec/2, exec/3
         ,get_user_vars/1
         ,set_user_vars/2
        ]).

-include("kzt.hrl").

-spec exec(kapps_call:call(), list()) -> exec_return().
-spec exec(kapps_call:call(), list(), api_binary() | list()) -> exec_return().
exec(Call, Cmds) ->
    exec(Call, Cmds, <<"text/xml">>).

exec(Call, Cmds, 'undefined') ->
    exec(Call, Cmds, <<"text/xml">>);
exec(Call, Cmds, CT) when not is_binary(CT) ->
    exec(Call, Cmds, kz_util:to_binary(CT));
exec(Call, Cmds, CT) ->
    case [{M, Cmd1} || M <- find_candidate_translators(just_the_type(CT)),
                       begin {IsRecognized, Cmd1} = is_recognized(M, Cmds), IsRecognized end
         ] of
        [] -> throw({'error', 'unrecognized_cmds'});
        [{Translator, Cmds1}|_] -> Translator:exec(Call, Cmds1)
    end.

-spec just_the_type(ne_binary()) -> ne_binary().
just_the_type(ContentType) ->
    case binary:split(ContentType, <<";">>) of
        [ContentType] -> kz_util:strip_binary(ContentType);
        [JustContentType | _Other]=L ->
            lager:debug("just using content type ~s, ignoring ~p", L),
            kz_util:strip_binary(JustContentType)
    end.

-spec get_user_vars(kapps_call:call()) -> kz_json:object().
-spec set_user_vars(kz_proplist(), kapps_call:call()) -> kapps_call:call().
get_user_vars(Call) ->
    ReqVars = kzt_util:get_request_vars(Call),
    UserVars = kapps_call:kvs_fetch(?KZT_USER_VARS, kz_json:new(), Call),
    kz_json:merge_jobjs(ReqVars, UserVars).
set_user_vars(Prop, Call) ->
    UserVars = get_user_vars(Call),
    kapps_call:kvs_store(?KZT_USER_VARS, kz_json:set_values(Prop, UserVars), Call).

-spec find_candidate_translators(ne_binary()) -> atoms().
find_candidate_translators(<<"text/xml">>) ->
    ['kzt_twiml'];
find_candidate_translators(<<"application/xml">>) ->
    ['kzt_twiml'];
find_candidate_translators(<<"application/json">>) ->
    ['kzt_kazoo'];
find_candidate_translators(_) ->
    ['kzt_twiml', 'kzt_kazoo'].

-spec is_recognized(atom(), any()) -> {boolean(), any()}.
is_recognized(M, Cmds) ->
    case catch M:parse_cmds(Cmds) of
        {'error', _E} -> {'false', []};
        {'ok', Resp} -> {'true', Resp}
    end.
