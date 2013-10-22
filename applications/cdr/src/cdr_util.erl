%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
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
    <<(wh_util:to_binary(Year))/binary
      ,(wh_util:pad_month(Month))/binary
      ,"-"
      ,CallId/binary>>.

-spec save_cdr(api_binary(), wh_json:object()) -> 
                      'ok' | wh_std_return().
save_cdr(AccountMOD, Doc) ->
    save_cdr(AccountMOD, Doc, 0).

-spec save_cdr(api_binary(), wh_json:object(), 0..?MAX_RETRIES) -> 
                      {'error', any()} | 'ok'.
save_cdr(_, _, ?MAX_RETRIES) -> 
    {'error', 'max_retries'};
save_cdr(AccountMODb, Doc, Retries) ->
    case couch_mgr:save_doc(AccountMODb, Doc) of
        {'error', 'not_found'} ->
            _ = couch_mgr:db_create(AccountMODb),
            save_cdr(AccountMODb, Doc, Retries + 1);
        {'error', _E} -> 
            lager:error("Account MODd Create Error: ~p", [_E]),
            save_cdr(AccountMODb, Doc, Retries + 1);
        {'ok', _} -> 'ok'
    end.
