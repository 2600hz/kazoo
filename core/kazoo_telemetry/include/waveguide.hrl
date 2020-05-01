%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(WAVEGUIDE_HRL).

-include("kazoo_telemetry.hrl").

-define(DAY_IN_SECONDS, 86400).
-define(DAY_IN_MS, ?DAY_IN_SECONDS * 1000).

-define(WG_CAT, <<(?TELEMETRY_CAT)/binary,".waveguide">>).
-define(WG_GRACE_PERIOD, 604800).
-define(WG_URL, <<"https://telemetry.2600hz.org/api">>).
-define(WG_USER_AGENT, "waveguide/" ++ ?WG_VERSION).
-define(WG_VERSION, "0.1.0").

-define(WG_ACTIVATION_PING, <<"activation">>).
-define(WG_HANDSHAKE_PING, <<"handshake">>).
-define(WG_MAIN_PING, <<"main">>).

-define(ANONYMIZE_HASH_ALG, 'sha').
-define(ANONYMIZE(Binary), kz_binary:hexencode(crypto:hash(?ANONYMIZE_HASH_ALG, Binary))).


-define(WG_ACTIVATION
       ,kapps_config:get_integer(?WG_CAT, <<"activation">>, kz_time:current_tstamp(), <<"default">>)
       ).

-define(is_valid_ping(Ping), Ping =:= ?WG_ACTIVATION_PING
        orelse Ping =:= ?WG_HANDSHAKE_PING
        orelse Ping =:= ?WG_MAIN_PING
       ).

-define(WAVEGUIDE_HRL, 'true').
-endif.
