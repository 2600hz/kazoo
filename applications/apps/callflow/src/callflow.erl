-module ( callflow ).

-export ( [start/0, start_link/0, stop/0] ).

start ( ) -> application:start(callflow).

start_link ( ) ->
   start_deps(),
   callflow_sup:start_link()
.

stop ( ) -> application:stop(callflow).

start_deps ( ) ->
   whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
   ensure_started(sasl),
   ensure_started(crypto),
   ensure_started(inets),
   ensure_started(whistle_amqp),
   ensure_started(whistle_couch).

ensure_started ( App ) ->
   case application:start(App) of
      ok -> ok;
      {error, {already_started, App}} -> ok
   end
.


