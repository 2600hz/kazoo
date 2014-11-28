-module(gen_smtp_application).

-export([ensure_all_started/1]).


-spec ensure_all_started(atom()) -> ok | {error, term()}.
ensure_all_started(App) ->
    start_ok(App, application:start(App, permanent)).

-spec start_ok(atom(), ok | {error, term()}) -> ok | {error, {term(), atom()}}.
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = ensure_all_started(Dep),
    ensure_all_started(App);
start_ok(App, {error, Reason}) ->
    {error, {Reason, App}}.
