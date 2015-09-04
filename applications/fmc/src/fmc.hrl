%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-ifndef(FMC_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"fmc">>).
-define(FMC_ROUTE_REQ_SECTION, <<"fmc_route_req_section">>).
-define(FMC_ROUTE_WIN_SECTION, <<"fmc_route_win_section">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(FMC_CONFIG_CAT, <<"fmc">>).
-define(PLATFORM_ORIGINATOR, whapps_config:get_integer(?FMC_CONFIG_CAT, <<"platform_origiantor_type">>, <<"FMC">>)).
-define(CCV, <<"Custom-Channel-Vars">>).

-define(XFMC_HEADER, whapps_config:get(<<"fmc">>, <<"x_fmc_header">>)).
-define(XFMC_REGEXP, whapps_config:get(<<"fmc">>, <<"x_fmc_regexp">>)).

-define(FMC_HRL, 'true').
-endif.
