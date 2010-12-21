%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_resource).

-export([init/1, to_html/2, encodings_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {"<html><head><script type='text/javascript' src='/assets/js/main.js'></script></head><body>Hello, new world</body></html>", ReqData, State}.

encodings_provided(RD, Context) ->
    { [
       {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}
      ]
      ,RD, Context}.
