-module(bh_auth).
-include("blackhole.hrl").
-export([init/0, authenticate/3]).

init() -> blackhole_bindings:bind(<<"authenticate">>, ?MODULE, 'authenticate').

authenticate(Context=#bh_context{auth_token = <<"">>}, <<"authenticate">>, _JMsg) -> Context;
authenticate(#bh_context{auth_token = <<"">>}, _Cmd, _JMsg) -> {'error', 'not_authenticated'};
authenticate(Context=#bh_context{}, _Cmd, _JMsg) -> Context.
