%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Standard interface for client modules to use to get dialplan commands
%%% translated into 2600Hz-specific commands
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_translator).

-export([exec/4
        ,get_user_vars/1
        ,set_user_vars/2
        ]).

-include("kzt.hrl").

-spec exec(kz_term:ne_binary(), kapps_call:call(), binary(), kz_term:api_binary() | list()) ->
          exec_return().
exec(RequesterQ, Call, 'undefined', Cmds) ->
    exec(RequesterQ, Call, <<"text/xml">>, Cmds);
exec(RequesterQ, Call, CT, Cmds) when not is_binary(CT) ->
    exec(RequesterQ, Call, kz_term:to_binary(CT), Cmds);
exec(RequesterQ, Call, CT, Cmds) ->
    case [{M, Cmd1} || M <- find_candidate_translators(just_the_type(CT)),
                       begin {IsRecognized, Cmd1} = is_recognized(M, Cmds), IsRecognized end
         ] of
        [] -> throw({'error', 'unrecognized_cmds'});
        [{Translator, Cmds1}|_] ->
            _ = publish_processing(RequesterQ, Call),
            Translator:exec(Call, Cmds1)
    end.

publish_processing(RequesterQ, Call) ->
    PubFun = fun(P) -> kapi_pivot:publish_processing(RequesterQ, P) end,
    kz_amqp_worker:cast([{<<"Call-ID">>, kapps_call:call_id(Call)}
                         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                        ]
                       ,PubFun
                       ).

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
