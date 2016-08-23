-module(bh_auth).
-include("blackhole.hrl").
-export([init/0, authenticate/2]).

init() -> blackhole_bindings:bind(<<"authenticate">>, ?MODULE, 'authenticate').

authenticate(#bh_context{auth_token = <<"">>}, _JMsg) -> {'error', 'not_authenticated'};
authenticate(Context=#bh_context{}, _JMsg) -> Context.