%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_default_handler).

-behaviour(cowboy_http_handler).

-export([init/3
         ,upgrade/4
         ,handle/2
         ,terminate/3
        ]).

-include("crossbar.hrl").

-spec init({atom(), 'http'}, cowboy_req:req(), wh_proplist()) ->
                  {'ok', cowboy_req:req(), 'undefined'}.
init({_Any, 'http'}, Req0, HandlerOpts) ->
    put('callid', ?LOG_SYSTEM_ID),

    {Path, Req1} = cowboy_req:path(Req0),

    case get_magic_token(Path) of
        'undefined' ->
            {'ok', Req1, 'undefined'};
        Magic ->
            lager:debug("magic path found: ~s", [Magic]),
            {'upgrade', 'protocol', ?MODULE, Req1, [{'magic_path', Magic} | HandlerOpts]}
    end.

upgrade(Req, Env, _Handler, HandlerOpts) ->
    cowboy_rest:upgrade(Req
                        ,props:set_value('handler', 'api_resource', Env)
                        ,'api_resource'
                        ,HandlerOpts
                       ).

get_magic_token(Path) ->
    get_magic_token_from_path(binary:split(Path, <<"/">>, ['global'])).

get_magic_token_from_path([]) -> 'undefined';
get_magic_token_from_path([Path|Paths]) ->
    case decode_magic_token(Path) of
        'undefined' -> get_magic_token_from_path(Paths);
        Token -> Token
    end.

decode_magic_token(<<>>) ->
    'undefined';
decode_magic_token(Token) ->
    try wh_util:from_hex_binary(Token) of
        Bin ->  decode_magic_token_bin(Bin)
    catch
        _E:_R -> 'undefined'
    end.

decode_magic_token_bin(Bin) ->
    try zlib:unzip(Bin) of
        Path -> Path
    catch
        _E:_R -> 'undefined'
    end.

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    lager:debug("default handler executing"),
    Headers = [{<<"Content-Type">>, <<"text/plain; charset=UTF-8">>}],
    Path = code:priv_dir('crossbar') ++ "/kazoo.txt",
    {'ok', Bytes} = file:read_file(Path),
    {'ok', Req1} = cowboy_req:reply(200, Headers, Bytes, Req),
    {'ok', Req1, State}.

-spec terminate(term(), cowboy_req:req(), term()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.
