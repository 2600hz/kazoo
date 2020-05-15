%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_utils).

-export([delete_doc/3]).

%%------------------------------------------------------------------------------
%% @doc Delete a doc from kazoo
%% Verify the doc type matches the DocType supplied
%% @end
%%------------------------------------------------------------------------------
-spec delete_doc(kz_type:ne_binary(), kz_type:ne_binary(), kz_type:ne_binary()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_doc(AccountId, DocId, DocType) ->
    AccountDb = kzs_util:format_account_id(AccountId, 'encoded'),
    case verify_doc_type(AccountDb, DocId, DocType) of
        {'error', Cause} = Error ->
            lager:error("failed to verify doc type when deleting doc, Cause: ~p, AccountId: ~p, DocId: ~p", [Cause, AccountId, DocId]),
            Error;
        'false' ->
            lager:error("doc id supplied is not correct type, AccountId: ~p, Doc id: ~p, Expected Type: ~p", [AccountId, DocId, DocType]),
            {'error', <<"doc id supplied is not the correct type">>};
        'true' ->
            do_delete_doc(AccountDb, DocId)
    end.

%%------------------------------------------------------------------------------
%% @doc For a given Account Db and Doc id, verify its type matches DocType
%% @end
%%------------------------------------------------------------------------------
-spec verify_doc_type(kz_type:ne_binary(), kz_type:ne_binary(), kz_type:ne_binary()) -> boolean() | {'error', kz_type:ne_binary()}.
verify_doc_type(AccountDb, DocId, DocType) ->
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'error', Cause} ->
            {'error', kz_term:to_binary(Cause)};
        {'ok', Doc} ->
            kz_doc:type(Doc) == DocType
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a doc from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec do_delete_doc(kz_type:ne_binary(), kz_type:ne_binary()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
do_delete_doc(AccountDb, DocId) ->
    case kz_datamgr:del_doc(AccountDb, DocId) of
        {'error', Cause} ->
            lager:error("failed to del doc, Cause: ~p, Doc id: ~p", [Cause, DocId]),
            {'error', kz_term:to_binary(Cause)};
        {'ok', _Doc} = Ok ->
            Ok
    end.
