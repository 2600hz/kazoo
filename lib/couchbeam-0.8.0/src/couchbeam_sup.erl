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
    Uuids = {couchbeam_uuids,
                {couchbeam_uuids, start_link, []},
	            permanent,2000,worker, [couchbeam_uuids]},

    {ok, {{one_for_one, 10, 3600}, [Uuids]}}.
