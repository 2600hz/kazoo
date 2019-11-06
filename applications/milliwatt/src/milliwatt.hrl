%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(MILLIWATT_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"milliwatt">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(DEFAULT_TONE, kz_json:from_list(
                        [{<<"caller_id">>,[]}
                        ,{<<"number">>,[<<"5555555551">>]}
                        ])
       ).
-define(TONE, kapps_config:get_json(?CONFIG_CAT, <<"tone">>, ?DEFAULT_TONE)).

-define(MILLIWATT_HRL, 'true').
-endif.
