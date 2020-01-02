%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Utility module for CDR operations
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cdr_util).

-export([get_cdr_doc_id/2
        ,get_cdr_doc_id/3

        ,register_views/0
        ]).
-export([save_cdr/2]).

-include("cdr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_cdr_doc_id(pos_integer(), kz_term:api_binary()) -> kz_term:ne_binary().
get_cdr_doc_id(Timestamp, CallId) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    get_cdr_doc_id(Year, Month, CallId).

-spec get_cdr_doc_id(pos_integer(), pos_integer(), kz_term:api_binary()) -> kz_term:ne_binary().
get_cdr_doc_id(Year, Month, CallId) ->
    kzd_cdrs:create_doc_id(CallId, Year, Month).

-spec save_cdr(kz_term:api_binary(), kz_json:object()) -> 'ok' | {'error', 'max_save_retries'}.
save_cdr(?KZ_ANONYMOUS_CDR_DB=Db, Doc) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"store_anonymous">>, 'false') of
        'false' -> lager:debug("ignoring storage for anonymous cdr");
        'true' -> do_save_cdr(Db, Doc)
    end;
save_cdr(AccountMOD, Doc) ->
    do_save_cdr(AccountMOD, Doc).

-spec do_save_cdr(kz_term:api_binary(), kz_json:object()) -> 'ok' | {'error', 'max_save_retries'}.
do_save_cdr(AccountMODb, Doc) ->
    case kazoo_modb:save_doc(AccountMODb, Doc, [{'max_retries', 3}]) of
        {'ok', _}-> 'ok';
        {'error', 'conflict'} -> 'ok';
        {'error', _E} ->
            lager:debug("failed to save cdr ~s : ~p", [kz_doc:id(Doc), _E]),
            {'error', 'max_save_retries'}
    end.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('cdr').
