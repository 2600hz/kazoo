%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_ensure_rates_in_syscfg).

-include("kamdb.hrl").

%% API
-export([init/1
         ,start_link/0
        ]).

-define(ACCOUNT_RATES_SEC, [{<<"registrations">>, 20}
                            ,{<<"invites">>, 50}
                            ,{<<"total">>, 100}
                           ]).
-define(DEVICE_RATES_SEC, [{<<"registrations">>, 2}
                           ,{<<"invites">>, 5}
                           ,{<<"total">>, 20}
                           ]).
-define(ACCOUNT_RATES_MIN, [{<<"registrations">>, 100}
                            ,{<<"invites">>, 200}
                            ,{<<"total">>, 2000}
                           ]).
-define(DEVICE_RATES_MIN, [{<<"registrations">>, 20}
                           ,{<<"invites">>, 100}
                           ,{<<"total">>, 1000}
                           ]).

-spec sysconfig_default_rates() -> wh_json:object().
sysconfig_default_rates() ->
    AccountSec = wh_json:from_list(?ACCOUNT_RATES_SEC),
    AccountMin = wh_json:from_list(?ACCOUNT_RATES_MIN),
    AccountRates = wh_json:from_list([{?MINUTE, AccountMin}
                                      ,{?SECOND, AccountSec}
                                     ]),

    DeviceSec = wh_json:from_list(?DEVICE_RATES_SEC),
    DeviceMin = wh_json:from_list(?DEVICE_RATES_MIN),
    DeviceRates = wh_json:from_list([{?MINUTE, DeviceMin}
                                      ,{?SECOND, DeviceSec}
                                     ]),

    wh_json:from_list([{<<"account">>, AccountRates}
                       ,{<<"device">>, DeviceRates}
                      ]).

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

init(_) ->
    _ = whapps_config:get(?APP_NAME, <<"rate_limits">>, sysconfig_default_rates()),
    'ignore'.
