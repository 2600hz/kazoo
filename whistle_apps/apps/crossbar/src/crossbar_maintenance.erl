%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([flush/0]).
-export([refresh/0, refresh/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the crossbar local cache
%% @end
%%--------------------------------------------------------------------
-spec flush/0 :: () -> 'ok'.
flush() ->
    {ok, Srv} = crossbar_sup:cache_proc(),
    wh_cache:flush_local(Srv).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> nonempty_string().
-spec refresh/1 :: (term()) -> iolist().

refresh() ->
    "please use whapps_maintenance:refresh().".

refresh(Value) ->
    io_lib:format("please use whapps_maintenance:refresh(~p).", [Value]).
