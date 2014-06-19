%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(web_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", cowboy_static, {priv_dir, web_server, "", [
				{mimetypes, cow_mimetypes, all},
				{dir_handler, directory_handler}
			]}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]},
		{middlewares, [cowboy_router, directory_lister, cowboy_handler]}
	]),
	web_server_sup:start_link().

stop(_State) ->
	ok.
