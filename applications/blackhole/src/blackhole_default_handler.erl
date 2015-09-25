%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(blackhole_default_handler).

%-behaviour(cowboy_http_handler).

-export([init/3
         ,handle/2
         ,terminate/3
        ]).

-include("blackhole.hrl").


-spec init({atom(), 'http'}, cowboy_req:req(), wh_proplist()) ->
                  {'ok', cowboy_req:req(), 'undefined'}.
init({_Any, 'http'}, Req0, _HandlerOpts) ->
    wh_util:put_callid(?LOG_SYSTEM_ID),
    {'ok', Req0, 'undefined'}.

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    Headers = [{<<"Content-Type">>, <<"text/plain; charset=UTF-8">>}],
    {'ok', Req1} = cowboy_req:reply(200, Headers, <<"OK">>, Req),
    {'ok', Req1, State}.

-spec terminate(term(), cowboy_req:req(), term()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.
