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

-define(FMC_HRL, 'true').
-endif.
