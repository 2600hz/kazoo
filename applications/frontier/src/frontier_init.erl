%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(frontier_init).

-include("frontier.hrl").

%% API
-export([start_link/0
        ,default_rate_limits/0
        ]).

-define(ACCOUNT_RATES_SEC, [{<<"registrations">>, 20}
                           ,{<<"invites">>, 50}
                           ,{<<"total_packets">>, 100}
                           ]).
-define(DEVICE_RATES_SEC, [{<<"registrations">>, 2}
                          ,{<<"invites">>, 5}
                          ,{<<"total_packets">>, 20}
                          ]).
-define(ACCOUNT_RATES_MIN, [{<<"registrations">>, 100}
                           ,{<<"invites">>, 200}
                           ,{<<"total_packets">>, 2000}
                           ]).
-define(DEVICE_RATES_MIN, [{<<"registrations">>, 20}
                          ,{<<"invites">>, 100}
                          ,{<<"total_packets">>, 1000}
                          ]).

-define(ACCOUNT_RATES
       ,kz_json:from_list([{?MINUTE, kz_json:from_list(?ACCOUNT_RATES_MIN)}
                          ,{?SECOND, kz_json:from_list(?ACCOUNT_RATES_SEC)}
                          ])).

-define(DEVICE_RATES
       ,kz_json:from_list([{?MINUTE, kz_json:from_list(?DEVICE_RATES_MIN)}
                          ,{?SECOND, kz_json:from_list(?DEVICE_RATES_SEC)}
                          ])).

-define(DEFAULT_RATES
       ,kz_json:from_list([{<<"account">>, ?ACCOUNT_RATES}
                          ,{<<"device">>, ?DEVICE_RATES}
                          ])).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = kz_process:spawn(fun default_rate_limits/0),
    'ignore'.

-spec default_rate_limits() -> kz_json:object().
default_rate_limits() ->
    kapps_config:get_json(?APP_NAME, <<"rate_limits">>, ?DEFAULT_RATES).
