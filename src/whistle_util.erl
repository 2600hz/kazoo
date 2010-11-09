-module(whistle_util).

-export([reload_all_apps/0, reload_app/1]).

reload_all_apps() ->
    Apps = application:which_applications(),
    lists:foreach(fun({App, _Desc, _Vsn}) -> reload_app(App) end, Apps).

reload_app(stdlib) -> ok;
reload_app(kernel) -> ok;
reload_app(App) ->
    {ok, Prop} = application:get_all_key(App),
    Mods = proplist:get_value(module, Prop, []),
    lists:foreach(fun(M) ->
			  code:purge(M),
			  code:load_file(M)
		  end, Mods).
