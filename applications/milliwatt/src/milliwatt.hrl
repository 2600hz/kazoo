%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-ifndef(MILLIWATT_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_api.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"milliwatt">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(MILLIWATT_HRL, 'true').
-endif.
