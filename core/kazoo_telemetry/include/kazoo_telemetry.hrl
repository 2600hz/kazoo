%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(KAZOO_TELEMETRY_HRL).

-define(TELEMETRY_CAT, <<"telemetry">>).
-define(TM_LEADER_TICK, 60000).
-define(TM_LEADER_APP, <<"kazoo_telemetry">>).
-define(TM_RESPONDERS, [<<"waveguide_responder">>]).

-define(ANONYMIZE_CLUSTER
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"cluster_id_anonymized">>, 'true', <<"default">>)
       ).

-define(INCLUDE_APPS
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"include_apps">>, 'true', <<"default">>)
       ).
-define(INCLUDE_DATABASE
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"include_database">>, 'true', <<"default">>)
       ).
-define(INCLUDE_FREESWITCH
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"include_freeswitch">>, 'true', <<"default">>)
       ).
-define(INCLUDE_KAMAILIO
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"include_kamailio">>, 'true', <<"default">>)
       ).
-define(INCLUDE_SERVICES
       ,kapps_config:get_boolean(?TELEMETRY_CAT, <<"include_services">>, 'true', <<"default">>)
       ).

-define(TM_COLLECTION_INTERVAL
       ,kapps_config:get_integer(?TELEMETRY_CAT, <<"collection_interval_s">>, 86400, <<"default">>)
       ).

-define(KAZOO_TELEMETRY_HRL, 'true').
-endif.
