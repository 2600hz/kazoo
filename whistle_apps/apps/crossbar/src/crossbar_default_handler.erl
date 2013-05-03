%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
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

-spec init({atom(), 'http'}, cowboy_req:req(), wh_proplist()) -> {'ok', cowboy_req:req(), 'undefined'}.
init({_Any, 'http'}, Req, []) ->
    {'ok', Req, 'undefined'}.

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    Headers = [{'Content-Type', "text/plain; charset=UTF-8"}],
    {'ok', Req1} = cowboy_http_req:chunked_reply(200, Headers, Req),
    Path = code:priv_dir('crossbar') ++ "/kazoo.txt",
    {'ok', Fd} = file:open(Path, 'read'),
    _ = readme_lines(Fd, Req1),
    {'ok', Req1, State}.

-spec terminate(term(), cowboy_req:req(), term()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.

-spec readme_lines(file:fd(), cowboy_req:req()) -> 'ok' | {'error', _}.
readme_lines(Fd, Req) ->
    case file:read_line(Fd) of
        {'ok', Data} ->
            _ = cowboy_http_req:chunk(Data, Req),
            timer:sleep(crypto:rand_uniform(15,250)),
            readme_lines(Fd, Req);
        _Else ->
            file:close(Fd)
    end.
