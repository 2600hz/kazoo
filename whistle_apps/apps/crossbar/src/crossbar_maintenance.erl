%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  13 jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([refresh/0, refresh/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> nonempty_string().
-spec refresh/1 :: (term()) -> nonempty_string().

refresh() ->
    "please use whapps_maintenance:refresh().".

refresh(Value) ->
    io_lib:format("please use whapps_maintenance:refresh(~p).", [Value]).
