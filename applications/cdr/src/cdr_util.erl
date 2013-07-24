%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%% Utility module for CDR operations
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_util).

%% API
-export([save_cdr/2
	 ,save_in_anonymous_cdrs/1
	 ,get_cdr_doc_id/2
	]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_cdr_doc_id(pos_integer(), pos_integer()) -> ne_binary().
get_cdr_doc_id(Year, Month) ->
    <<(wh_util:to_binary(Year))/binary, (wh_util:pad_month(Month))/binary, "-", (couch_mgr:get_uuid())/binary>>.


-spec save_cdr(api_binary(), wh_json:object()) -> 'ok'.
save_cdr(AccountMOD, Doc) ->
    save_cdr(AccountMOD, Doc, 0).

-spec save_cdr(api_binary(), wh_json:object(), 0..?MAX_RETRIES) -> 
                      {'error', 'max_retries'} | 'ok'.
save_cdr(_, _, ?MAX_RETRIES) -> {'error', 'max_retries'};

save_cdr(AccountMOD, Doc, Retries) ->
    case couch_mgr:save_doc(AccountMOD, Doc) of
        {'error', 'not_found'} ->
            couch_mgr:db_create(AccountMOD),
            save_cdr(AccountMOD, Doc, Retries);
        {'ok', _} -> 'ok';
        {'error', _} -> save_cdr(AccountMOD, Doc, Retries+1)
    end.

-spec save_in_anonymous_cdrs(wh_json:object()) -> 'ok'.
save_in_anonymous_cdrs(JObj) ->
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
            ],
    J = wh_doc:update_pvt_parameters(JObj, ?WH_ANONYMOUS_CDR_DB, Props),
    case couch_mgr:save_doc(?WH_ANONYMOUS_CDR_DB, J) of
        {'error', 'not_found'} ->
            'undefined' = get('attempted_db_create'),
            _ = create_anonymous_cdr_db(),
            put('attempted_db_create', 'true'),
            save_in_anonymous_cdrs(JObj);
        {'error', 'conflict'} -> 'ok';
        {'ok', _} -> 'ok'
    end.

-spec create_anonymous_cdr_db() -> {'ok', wh_json:object()} |
                                   {'error', term()}.
create_anonymous_cdr_db() ->
    couch_mgr:db_create(?WH_ANONYMOUS_CDR_DB),
    couch_mgr:revise_doc_from_file(?WH_ANONYMOUS_CDR_DB, 'cdr', <<"cdr.json">>).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
