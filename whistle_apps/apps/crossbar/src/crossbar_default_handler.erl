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
-export([terminate/2]).

-include("crossbar.hrl").

-spec init({atom(), 'http'}, #http_req{}, wh_proplist()) -> {'ok', #http_req{}, 'undefined'}.
init({_Any, 'http'}, Req, []) ->    
    {'ok', Req, 'undefined'}.

-spec handle(#http_req{}, State) -> {'ok', #http_req{}, State}.
handle(Req, State) ->
    {'ok', Req1} = cowboy_http_req:reply(200, [], <<"Howdy, new world!\n">>, Req),
    {'ok', Req1, State}.

-spec terminate(#http_req{}, term()) -> 'ok'.
terminate(_Req, _State) -> 'ok'.


