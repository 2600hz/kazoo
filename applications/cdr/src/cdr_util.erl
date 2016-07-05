%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2014, 2600Hz
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
    <<(kz_util:to_binary(Year))/binary
      ,(kz_util:pad_month(Month))/binary
      ,"-"
      ,CallId/binary
    >>.

-spec save_cdr(api_binary(), kz_json:object()) ->
                      'ok' | kz_std_return().
save_cdr(?KZ_ANONYMOUS_CDR_DB=Db, Doc) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"store_anonymous">>, 'false') of
        'false' -> lager:debug("ignoring storage for anonymous cdr");
        'true' -> save_cdr(Db, Doc, 0)
    end;
save_cdr(AccountMOD, Doc) ->
    save_cdr(AccountMOD, Doc, 0).

-spec save_cdr(api_binary(), kz_json:object(), 0..?MAX_RETRIES) ->
                      {'error', any()} | 'ok'.
save_cdr(_, _, ?MAX_RETRIES) ->
    {'error', 'max_retries'};
save_cdr(AccountMODb, Doc, Retries) ->
    case kz_datamgr:save_doc(AccountMODb, Doc) of
        {'ok', _}-> 'ok';
        {'error', 'not_found'} ->
            _ = kazoo_modb:create(AccountMODb),
            save_cdr(AccountMODb, Doc, Retries + 1);
        {'error', 'conflict'} -> 'ok';
        {'error', _E} ->
            lager:debug("Account MODb Create Error: ~p", [_E])
    end.
