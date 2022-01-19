%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_media_init).

-include("kazoo_media.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"fix_media_names">>, 'true') of
        'true' ->
            kz_datamgr:suppress_change_notice(),
            kazoo_media_maintenance:fix_media_names(),
            'ignore';
        'false' -> 'ignore'
    end.
