-ifndef(KAZOO_DOCUMENTS_HRL).

-define(ACQUIRED_UUID, <<"Acquired-UUID">>).
-define(RESIGNING_UUID, <<"Resigning-UUID">>).

-define(VM_FOLDER_NEW, <<"new">>).
-define(VM_FOLDER_SAVED, <<"saved">>).
-define(VM_FOLDER_DELETED, <<"deleted">>).

-define(VM_KEY_MESSAGES, <<"messages">>).
-define(VM_KEY_FOLDER, <<"folder">>).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(VERSION_1, <<"v1">>).
-define(VERSION_2, <<"v2">>).
-define(VERSION_SUPPORTED, [?VERSION_1, ?VERSION_2]).
-define(CURRENT_VERSION, ?VERSION_2).

-define(KAZOO_DOCUMENTS_HRL, 'true').

-type doc_validation_error() :: {kz_json:path(), kz_term:ne_binary(), kz_json:object()}.
-type doc_validation_errors() :: [doc_validation_error()].
-type doc_validation_acc() :: {kz_doc:doc(), doc_validation_errors()}.
-type doc_validation_fun() :: fun((kz_term:api_ne_binary(), doc_validation_acc()) -> doc_validation_acc()).
-type doc_validation_after_fun() :: fun((doc_validation_acc()) -> doc_validation_acc()) | 'undefined'.
-type doc_validation_return() :: {'true', kz_doc:doc()} |
                                 {'validation_errors', doc_validation_errors()} |
                                 {'system_error', atom() | {atom(), kz_term:ne_binary()}}.
-export_type([doc_validation_error/0, doc_validation_errors/0
             ,doc_validation_acc/0, doc_validation_fun/0
             ,doc_validation_after_fun/0
             ,doc_validation_return/0
             ]).
-endif.
