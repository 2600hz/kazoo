%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_init).

-include("kazoo_auth.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = init(),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> any().
init() ->
    case kz_datamgr:db_exists(?KZ_AUTH_DB) of
        'false' -> init(kz_datamgr:db_create(?KZ_AUTH_DB));
        'true' -> 'ok'
    end,
    kz_auth_keys:public_key(?SYSTEM_KEY_ID).

-spec init(boolean()) -> any().
init('false') ->
    lager:error("error trying to create auth database");
init('true') ->
    kazoo_auth_maintenance:register_common_providers(),
    kazoo_auth_maintenance:refresh().
