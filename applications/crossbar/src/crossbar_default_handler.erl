%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_default_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,upgrade/4
        ,handle/2
        ,terminate/3
        ]).

-include("crossbar.hrl").

-define(DEFAULT_PATHS, [<<"/:version/accounts/:account_id/vmboxes/:box_id/messages/:message_id/raw">>
                       ,<<"/:version/accounts/:account_id/faxes/:direction/:fax_id/attachment">>
                       ,<<"/:version/accounts/:account_id/presence/:reportid">>
                       ]).

-spec init(cowboy_req:req(), map()) ->
                  {'ok', cowboy_req:req(), map()} |
                  {?MODULE, cowboy_req:req(), kz_term:proplist()}.
init(Req, HandlerOpts) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Path = cowboy_req:path(Req),
    case get_magic_token(Path) of
        'undefined' -> handle(Req, HandlerOpts);
        Magic ->
            case is_valid_magic_path(Magic) of
                'true' ->
                    lager:debug("magic path found: ~s", [Magic]),
                    {?MODULE, Req, [{'magic_path', Magic}]};
                'false' ->
                    lager:debug("invalid magic path: ~s", [Magic]),
                    handle(Req, HandlerOpts)
            end
    end.

is_valid_magic_path(Path) ->
    is_valid_magic_path(Path, kapps_config:get(?CONFIG_CAT, <<"magic_path_patterns">>, ?DEFAULT_PATHS)).

is_valid_magic_path(Path, Templates) ->
    lists:any(fun(Template) -> path_matches_template(Path, Template) end, Templates).

path_matches_template(Path, Template) ->
    lager:debug("testing ~s against ~s", [Path, Template]),
    path_matches_template_tokens(binary:split(Path, <<"/">>, ['global'])
                                ,binary:split(Template, <<"/">>, ['global'])
                                ).

path_matches_template_tokens([], []) -> 'true';
path_matches_template_tokens([_|PathTokens], [<<":", _/binary>>|TemplateTokens]) ->
    path_matches_template_tokens(PathTokens, TemplateTokens);
path_matches_template_tokens([Token|PathTokens], [Token|TemplateTokens]) ->
    path_matches_template_tokens(PathTokens, TemplateTokens);
path_matches_template_tokens(_, _) -> 'false'.

-spec upgrade(cowboy_req:req(), cowboy_middleware:env(), any(), any()) -> {'ok', cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Req, Env, _Handler, HandlerOpts) ->
    NewEnv = maps:put('handler', 'api_resource', Env),
    cowboy_rest:upgrade(Req, NewEnv, 'api_resource', HandlerOpts).

get_magic_token(Path) ->
    get_magic_token_from_path(binary:split(Path, <<"/">>, ['global'])).

get_magic_token_from_path([]) -> 'undefined';
get_magic_token_from_path([<<>>|Paths]) -> get_magic_token_from_path(Paths);
get_magic_token_from_path([Path|Paths]) ->
    try kapps_util:from_magic_hash(Path)
    catch
        _:_ -> get_magic_token_from_path(Paths)
    end.

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, #{body := Bytes} = State) ->
    Headers = #{<<"content-type">> => <<"text/plain; charset=UTF-8">>},
    Req1 = cowboy_req:reply(200, Headers, Bytes, Req),
    {'ok', Req1, State}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.
