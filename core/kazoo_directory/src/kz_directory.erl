%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_directory).

-export([lookup/2, lookup/3]).

-include("kazoo_directory.hrl").

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
lookup(EndpointId, AccountId) ->
    lookup(EndpointId, AccountId, []).

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
lookup(EndpointId, AccountId, Options) ->
    case kz_directory_endpoint:profile(EndpointId, AccountId, Options) of
        {'ok', _Endpoint} = OK -> OK;
        _Error -> kz_directory_resource:profile(EndpointId, AccountId, Options)
    end.

