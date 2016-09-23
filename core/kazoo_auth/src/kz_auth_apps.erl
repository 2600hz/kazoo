%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_apps).

-include("kazoo_auth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_auth_app/1
        ]).


get_auth_app(AppId) ->
    case kz_datamgr:open_doc(?KZ_AUTH_DB, AppId) of
        {'ok', JObj} ->
            #{'_id' := Id} = Map = kz_auth_util:map_keys_to_atoms(kz_json:to_map(JObj)),
            Map#{name => Id};
        {'error', _} -> {'error', <<"AUTH - App ", AppId/binary, " not found">>}
    end.
