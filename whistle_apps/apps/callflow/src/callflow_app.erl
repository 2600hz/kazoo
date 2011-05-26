-module ( callflow_app ).

-behaviour ( application ).

-export ( [start/2, stop/1] ).

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start ( _StartType, _StartArgs ) ->
    case callflow:start_link() of
	{ok, P}                       -> { ok, P };
	{error, {already_started, P}} -> { ok, P };
	{error, _} = E                -> E
    end
.

stop ( _State ) -> ok.
