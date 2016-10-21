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
-include_lib("kazoo/include/kz_api_literals.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"milliwatt">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(TONE, kapps_config:get_non_empty(?CONFIG_CAT
                                        ,<<"tone">>
                                        ,kz_json:from_list(
                                           [{<<"caller_id">>,[]}
                                           ,{<<"number">>,[<<"5555555551">>]}
                                           ]))
       ).

-define(MILLIWATT_HRL, 'true').
-endif.
