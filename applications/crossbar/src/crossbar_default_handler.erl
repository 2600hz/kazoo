%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_default_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("crossbar.hrl").

-spec init({atom(), 'http'}, cowboy_req:req(), wh_proplist()) ->
                  {'ok', cowboy_req:req(), 'undefined'}.
init({_Any, 'http'}, Req, []) ->
    put('callid', ?LOG_SYSTEM_ID),
    {'ok', Req, 'undefined'}.

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    Headers = [{<<"Content-Type">>, <<"text/plain; charset=UTF-8">>}],
    Path = code:priv_dir('crossbar') ++ "/kazoo.txt",
    {'ok', Bytes} = file:read_file(Path),
    {'ok', Req1} = cowboy_req:reply(200, Headers, Bytes, Req),
    {'ok', Req1, State}.

-spec terminate(term(), cowboy_req:req(), term()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.
