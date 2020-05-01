%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_util).

-include("kazoo_auth.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([get_json_from_url/1, get_json_from_url/2]).
-export([fetch_access_code/2, fetch_access_code/3]).
-export([run/2]).

-spec get_json_from_url(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_json_from_url(Url) ->
    get_json_from_url(Url, []).

-spec get_json_from_url(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
get_json_from_url(Url, ReqHeaders) ->
    case kz_http:get(kz_term:to_list(Url), ReqHeaders, [{'ssl', [{'versions', ['tlsv1.2']}]}]) of
        {'ok', 200, _RespHeaders, Body} ->
            JObj = kz_json:decode(Body),
            case kz_term:is_empty(JObj) of
                'true' -> {'error', 'empty'};
                'false' -> {'ok', JObj}
            end;
        {'ok', Code, _RespHeaders, Body} ->
            lager:debug("unexpected code ~b downloading json from ~s : ~s", [Code, Url, Body]),
            {'error', 'unexpected_result'};
        Else ->
            lager:debug("error downloading json from ~s : ~p", [Url, Else]),
            {'error', Else}
    end.

-spec fetch_access_code(kz_term:ne_binary() | map(), kz_term:ne_binary() ) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
fetch_access_code(AppId, AuthorizationCode) ->
    fetch_access_code(AppId, AuthorizationCode, <<"postmessage">>).


-spec fetch_access_code(kz_term:ne_binary() | map(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
fetch_access_code(AppId, AuthorizationCode, RedirectUri)
  when is_binary(AppId) ->
    lager:debug("getting auth-app ~p",[AppId]),
    case kz_auth_apps:get_auth_app(AppId, app_and_provider) of
        #{} = Map -> fetch_access_code(Map, AuthorizationCode, RedirectUri);
        Error -> Error
    end;
fetch_access_code(#{auth_app := #{name := ClientId
                                 ,pvt_secret := Secret
                                 }
                   ,auth_provider := #{access_code_url := URL}
                   }, AuthorizationCode, RedirectUri) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],

    Fields = [{"client_id", ClientId}
             ,{"redirect_uri", RedirectUri}
             ,{"client_secret", Secret}
             ,{"grant_type", "authorization_code"}
             ,{"code", AuthorizationCode}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K, kz_term:to_list(V)], "=") ] end, Fields)),"&"),
    case kz_http:post(kz_term:to_list(URL), Headers, Body, [{'ssl', [{'versions', ['tlsv1.2']}]}]) of
        {'ok', 200, _RespHeaders, RespXML} -> {'ok', kz_json:decode(RespXML)};
        Else ->
            lager:error("~p", [Else]),
            {'error', Else}
    end.


%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec run(map(), list()) -> {'ok' | 'error', map()}.
run(Token, []) -> {'ok', Token};
run(Token, [Fun | Routines]) ->
    try Fun(Token) of
        #{error := _Err}=Error -> {'error', Error};
        {'ok', NewToken} -> run(NewToken, Routines);
        {'done', NewToken} -> {'ok', NewToken};
        {'error', Error} -> {'error', Token#{error => Error}};
        NewToken -> run(NewToken, Routines)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("exception executing ~p : ~p , ~p, ~p", [Fun, _E, _R, Token]),
        kz_log:log_stacktrace(ST),
        {'error', Token}
        end.
