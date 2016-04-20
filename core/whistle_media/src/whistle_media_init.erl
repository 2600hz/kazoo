%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(whistle_media_init).

-include("whistle_media.hrl").

-export([start_link/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case whapps_config:get_is_true(?WHM_CONFIG_CAT, <<"fix_media_names">>, 'true') of
        'true' ->
            kz_datamgr:suppress_change_notice(),
            whistle_media_maintenance:fix_media_names(),
             'ignore';
        'false' -> 'ignore'
    end.
