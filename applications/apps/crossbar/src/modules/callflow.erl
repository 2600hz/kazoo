-module(callflow).

-export([start_link/0]).

start_link() -> callflow_sup:start_link().
