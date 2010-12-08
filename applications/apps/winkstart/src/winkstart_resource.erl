%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(winkstart_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {"<html><head><script type='text/javascript' src='main.js'></script></head><body><img src='foo.jpg' alt='foo missing' /></body></html>", ReqData, State}.
