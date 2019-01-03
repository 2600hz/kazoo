%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Standard interface for client modules to use to get dialplan commands
%%% translated into 2600Hz-specific commands
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_translator).

-export([exec/2, exec/3
        ,get_user_vars/1
        ,set_user_vars/2
        ]).

-include("kzt.hrl").

-spec exec(kapps_call:call(), binary()) -> exec_return().
exec(Call, Cmds) ->
    exec(Call, Cmds, <<"text/xml">>).

-spec exec(kapps_call:call(), binary(), kz_term:api_binary() | list()) -> exec_return().
exec(Call, Cmds, 'undefined') ->
    exec(Call, Cmds, <<"text/xml">>);
exec(Call, Cmds, CT) when not is_binary(CT) ->
    exec(Call, Cmds, kz_term:to_binary(CT));
exec(Call, Cmds, CT) ->
    case [{M, Cmd1} || M <- find_candidate_translators(just_the_type(CT)),
                       begin {IsRecognized, Cmd1} = is_recognized(M, Cmds), IsRecognized end
         ] of
        [] -> throw({'error', 'unrecognized_cmds'});
        [{Translator, Cmds1}|_] -> Translator:exec(Call, Cmds1)
    end.

-spec just_the_type(kz_term:ne_binary()) -> kz_term:ne_binary().
just_the_type(ContentType) ->
    case binary:split(ContentType, <<";">>) of
        [ContentType] -> kz_binary:strip(ContentType);
        [JustContentType | _Other]=L ->
            lager:debug("just using content type ~s, ignoring ~p", L),
            kz_binary:strip(JustContentType)
    end.

-spec get_user_vars(kapps_call:call()) -> kz_json:object().
get_user_vars(Call) ->
    ReqVars = kzt_util:get_request_vars(Call),
    UserVars = kapps_call:kvs_fetch(?KZT_USER_VARS, kz_json:new(), Call),
    kz_json:merge_jobjs(ReqVars, UserVars).

-spec set_user_vars(kz_term:proplist(), kapps_call:call()) -> kapps_call:call().
set_user_vars(Prop, Call) ->
    UserVars = get_user_vars(Call),
    kapps_call:kvs_store(?KZT_USER_VARS, kz_json:set_values(Prop, UserVars), Call).

-spec find_candidate_translators(kz_term:ne_binary()) -> kz_term:atoms().
find_candidate_translators(<<"text/xml">>) ->
    ['kzt_twiml'];
find_candidate_translators(<<"application/xml">>) ->
    ['kzt_twiml'];
find_candidate_translators(<<"application/json">>) ->
    ['kzt_kazoo'];
find_candidate_translators(_) ->
    ['kzt_twiml', 'kzt_kazoo'].

-spec is_recognized(atom(), binary()) -> {boolean(), any()}.
is_recognized(M, Cmds) ->
    case catch M:parse_cmds(Cmds) of
        {'json', _Msg, _B, _A}=Err -> throw(Err);
        {'error', _E} -> {'false', []};
        {'ok', Resp} -> {'true', Resp}
    end.
