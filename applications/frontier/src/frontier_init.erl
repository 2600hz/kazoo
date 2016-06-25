%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(frontier_init).

-include_lib("frontier/src/frontier.hrl").


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

-spec sysconfig_default_rates() -> kz_json:object().
sysconfig_default_rates() ->
    AccountSec = kz_json:from_list(?ACCOUNT_RATES_SEC),
    AccountMin = kz_json:from_list(?ACCOUNT_RATES_MIN),

    AccountRates = kz_json:from_list([{?MINUTE, AccountMin}
                                      ,{?SECOND, AccountSec}
                                     ]),

    DeviceSec = kz_json:from_list(?DEVICE_RATES_SEC),
    DeviceMin = kz_json:from_list(?DEVICE_RATES_MIN),

    DeviceRates = kz_json:from_list([{?MINUTE, DeviceMin}
                                     ,{?SECOND, DeviceSec}
                                    ]),

    kz_json:from_list([{<<"account">>, AccountRates}
                       ,{<<"device">>, DeviceRates}
                      ]).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = kz_util:spawn(fun default_rate_limits/0),
    'ignore'.

-spec default_rate_limits() -> kz_json:object().
default_rate_limits() ->
    kapps_config:get(?APP_NAME, <<"rate_limits">>, sysconfig_default_rates()).
