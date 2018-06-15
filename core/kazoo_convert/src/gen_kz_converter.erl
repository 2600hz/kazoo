%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc File converter behaviour.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_kz_converter).

-type converted() :: {'ok', any()}|
                     {'ok', any(), kz_term:proplist()}|
                     {'error', any()}.

-callback convert(kz_term:api_binary()
                 ,kz_term:api_binary()
                 ,any()
                 ,kz_term:proplist()) -> any().

-callback read_metadata(kz_term:ne_binary()) -> kz_term:proplist().

-export_type([converted/0]).
