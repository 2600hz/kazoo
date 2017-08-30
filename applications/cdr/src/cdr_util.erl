%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2017, 2600Hz
%%% @doc
%%% Utility module for CDR operations
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_util).

-export([get_cdr_doc_id/2
        ,get_cdr_doc_id/3
        ]).
-export([save_cdr/2]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_cdr_doc_id(pos_integer(), api_binary()) -> ne_binary().
get_cdr_doc_id(Timestamp, CallId) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    get_cdr_doc_id(Year, Month, CallId).

-spec get_cdr_doc_id(pos_integer(), pos_integer(), api_binary()) -> ne_binary().
get_cdr_doc_id(Year, Month, CallId) ->
    <<(kz_term:to_binary(Year))/binary
      ,(kz_date:pad_month(Month))/binary
      ,"-"
      ,CallId/binary
    >>.

-spec save_cdr(api_binary(), kz_json:object()) -> 'ok' | {'error', max_save_retries}.
save_cdr(?KZ_ANONYMOUS_CDR_DB=Db, Doc) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"store_anonymous">>, 'false') of
        'false' -> lager:debug("ignoring storage for anonymous cdr");
        'true' -> do_save_cdr(Db, Doc)
    end;
save_cdr(AccountMOD, Doc) ->
    do_save_cdr(AccountMOD, Doc).

-spec do_save_cdr(api_binary(), kz_json:object()) -> 'ok' | {'error', max_save_retries}.
do_save_cdr(AccountMODb, Doc) ->
    case kazoo_modb:save_doc(AccountMODb, Doc, [{'max_retries', 3}]) of
        {'ok', _}-> 'ok';
        {'error', 'conflict'} -> 'ok';
        {'error', _E} ->
            lager:debug("failed to save cdr ~s : ~p", [kz_doc:id(Doc), _E]),
            {'error', max_save_retries}
    end.
