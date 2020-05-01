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
-define(VERSION_SUPPORTED, [?VERSION_2]).
-define(CURRENT_VERSION, ?VERSION_2).

-define(KAZOO_DOCUMENTS_HRL, 'true').
-endif.
