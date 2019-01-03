%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Callflow Action behaviour.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_cf_action).

-callback handle(kz_json:object(), kapps_call:call()) -> any().
