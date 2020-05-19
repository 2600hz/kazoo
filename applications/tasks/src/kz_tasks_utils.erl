%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-, Voxter
%%% @doc
%%% @author Ben Bradford
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_utils).

-export([delete_doc/3
        ,merge_kazoo_doc_validation_errors/1
        ,generate_new_doc_with_kazoo_doc_setters/2
        ]).

-define(KAZOO_DOC_ERROR_SEPARATOR, ",").

%%------------------------------------------------------------------------------
%% @doc Delete a doc from kazoo
%% Verify the doc type matches the DocType supplied
%% @end
%%------------------------------------------------------------------------------
-spec delete_doc(kz_type:ne_binary(), kz_type:ne_binary(), kz_type:ne_binary()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_doc(AccountId, DocId, DocType) ->
    AccountDb = kzs_util:format_account_db(AccountId),
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

%%------------------------------------------------------------------------------
%% @doc Convert multiple `kazoo_documents:doc_validation_error()' into a binary
%% string, Each error separated by `?KAZOO_DOC_ERROR_SEPARATOR'.
%% @end
%%------------------------------------------------------------------------------
-spec merge_kazoo_doc_validation_errors(kazoo_documents:doc_validation_errors()) -> kz_term:ne_binary().
merge_kazoo_doc_validation_errors(ValidationErrors) ->
    ErrorBinaries = lists:map(fun(ValidationError) -> kazoo_doc_validation_error_to_binary(ValidationError) end
                             ,ValidationErrors),
    kz_term:to_binary(lists:join(?KAZOO_DOC_ERROR_SEPARATOR, ErrorBinaries)).

%%------------------------------------------------------------------------------
%% @doc Convert `kazoo_documents:doc_validation_error()' into a binary string.
%% @end
%%------------------------------------------------------------------------------
-spec kazoo_doc_validation_error_to_binary(kazoo_documents:doc_validation_error()) -> kz_term:ne_binary().
kazoo_doc_validation_error_to_binary({Path, ErrorCode, JObj}) ->
    PathBin = kz_term:to_binary(lists:join(".", Path)),
    ErrorCodeBin = kz_term:to_binary(ErrorCode),
    MessageBin = kz_json:get_binary_value(<<"message">>, JObj),
    <<"Validation error '", ErrorCodeBin/binary, "' on field '", PathBin/binary, "' : ", MessageBin/binary>>.

%%------------------------------------------------------------------------------
%% @doc Build doc from default values and set / overwrite values defined
%% in the input Args (CSV row)
%% This uses `get_setters/0' from the supplied `KazooDocModule' to get a mapping
%% of doc attributes to setter functions for the document.
%% It then uses that map and applies any of the functions to a new default doc
%% where the `Args' value is not `undefined'.
%% @end
%%------------------------------------------------------------------------------
-spec generate_new_doc_with_kazoo_doc_setters(erlang:module(), kz_tasks:args()) -> kz_doc:doc().
generate_new_doc_with_kazoo_doc_setters(KazooDocModule, Args) ->
    KeyToSetFunMap = erlang:apply(KazooDocModule, 'get_setters', []),
    DefaultDoc = erlang:apply(KazooDocModule, 'new', []),
    SetterFun = fun(_Key, 'undefined', JObjAcc) -> JObjAcc;
                   (Key, Value, JObjAcc) ->
                        case maps:get(Key, KeyToSetFunMap, 'undefined') of
                            'undefined' ->
                                JObjAcc;
                            SetterFun ->
                                erlang:apply('kzd_users', SetterFun, [JObjAcc, Value])
                        end
                end,
    maps:fold(SetterFun, DefaultDoc, Args).
