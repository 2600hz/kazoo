%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchbeam_sup).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    AChild = {couchbeam,{couchbeam,start_link,[]},
	      permanent,2000,worker, [couchbeam]},
    
    {ok, {{one_for_one, 3, 10}, [AChild]}}.
