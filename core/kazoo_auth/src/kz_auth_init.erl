%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_init).

-include("kazoo_auth.hrl").

-export([start_link/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = init(),
    'ignore'.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures that all exchanges used are declared
%%--------------------------------------------------------------------
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
