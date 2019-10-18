%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Utilities for manipulating Kazoo documents.
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_doc).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export([id/1, id/2
        ,set_id/2
        ,path_id/0
        ]).
-export([revision/1
        ,revision_id/1
        ,set_revision/2
        ,delete_revision/1
        ,path_revision/0
        ]).
-export([type/1, type/2
        ,set_type/2
        ,path_type/0
        ]).
-export([account_id/1, account_id/2
        ,set_account_id/2
        ,path_account_id/0
        ]).
-export([account_db/1, account_db/2
        ,set_account_db/2
        ,path_account_db/0
        ]).
-export([created/1
        ,created/2
        ,set_created/2
        ,path_created/0
        ]).
-export([modified/1
        ,modified/2
        ,set_modified/2
        ,path_modified/0
        ]).
-export([vsn/1, vsn/2
        ,set_vsn/2
        ,path_vsn/0
        ]).
-export([document_hash/1
        ,set_document_hash/2
        ]).
-export([is_soft_deleted/1
        ,set_soft_deleted/2
        ]).
-export([is_deleted/1
        ,set_deleted/1
        ,set_deleted/2
        ]).
-export([attachments/1
        ,attachments/2
        ,stub_attachments/1
        ,stub_attachments/2
        ,external_attachments/1
        ,external_attachments/2
        ,attachment_names/1
        ,delete_attachments/1
        ,path_attachments/0
        ,path_external_attachments/0
        ]).
-export([attachment/1
        ,attachment/2
        ,attachment/3
        ,attachment_revision/1
        ,attachment_length/2
        ,attachment_length/3
        ,attachment_content_type/1
        ,attachment_content_type/2
        ,attachment_content_type/3
        ,attachment_property/3
        ,attachment_property/4
        ,delete_attachment/2
        ]).

-export([update_pvt_parameters/2
        ,update_pvt_parameters/3
        ,update_pvt_modified/1
        ,get_pvt_updates/3
        ]).

-export([public_fields/1
        ,public_fields/2
        ,get_public_keys/1
        ,private_fields/1
        ,is_private_key/1
        ,leak_private_fields/1
        ]).

-export([calculate_document_hash/1]).

-export([maybe_remove_attachments/1
        ,maybe_remove_attachments/2
        ,latest_attachment_id/1
        ,compare_attachments/2
        ]).

-export([setters/1
        ,setters/2
        ]).

-ifdef(TEST).
-export([remove_pvt/1]).
-endif.

-define(PVT_FUNS, [fun add_pvt_vsn/4
                  ,fun add_pvt_account_id/4
                  ,fun add_pvt_account_db/4
                  ,fun add_pvt_created/4
                  ,fun add_pvt_modified/4
                  ,fun add_pvt_type/4
                  ,fun add_pvt_node/4
                  ,fun add_id/4
                  ,fun add_pvt_document_hash/4
                  ]).

%% CouchDB Keys
-define(KEY_ATTACHMENTS, <<"_attachments">>).
-define(KEY_DELETED, <<"_deleted">>).
-define(KEY_ID, <<"_id">>).
-define(KEY_REV, <<"_rev">>).

%% Private Kazoo Keys
-define(KEY_ACCOUNT_DB, <<"pvt_account_db">>).
-define(KEY_ACCOUNT_ID, <<"pvt_account_id">>).
-define(KEY_CREATED, <<"pvt_created">>).
-define(KEY_MODIFIED, <<"pvt_modified">>).
-define(KEY_NODE, <<"pvt_node">>).
-define(KEY_PVT_TYPE, <<"pvt_type">>).
-define(KEY_SOFT_DELETED, <<"pvt_deleted">>).
-define(KEY_VSN, <<"pvt_vsn">>).
-define(KEY_EXTERNAL_ATTACHMENTS, <<"pvt_attachments">>).
-define(KEY_DOCUMENT_HASH, <<"pvt_document_hash">>).

-define(ATTACHMENT_PROPERTY_REVISION, <<"revpos">>).

%% Helper Macros
-define(KEYS_ATTACHMENTS, [?KEY_ATTACHMENTS, ?KEY_EXTERNAL_ATTACHMENTS]).
-define(KEYS_ATTACHMENTS(A), [ [Key, A] || Key <- ?KEYS_ATTACHMENTS]).

-type doc() :: kz_json:object().
-type setter_fun() :: {fun((doc(), Value) -> doc()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([doc/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(doc()) -> kz_term:api_binary().
id(JObj) ->
    id(JObj, 'undefined').

-spec id(doc(), Default) -> kz_term:ne_binary() | Default.
id(JObj, Default) ->
    Id = kz_json:get_first_defined([?KEY_ID
                                   ,<<"id">>
                                   ,<<"ID">>
                                   ,[<<"value">>, <<"id">>]
                                   ,[<<"doc">>, <<"_id">>]
                                   ]
                                  ,JObj
                                  ,Default
                                  ),
    case kz_term:is_empty(Id) of
        'true' -> Default;
        'false' -> Id
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_id(doc(), kz_term:ne_binary()) -> doc().
set_id(JObj, Id) ->
    kz_json:set_value(?KEY_ID, Id, JObj).

-spec path_id() -> kz_json:path().
path_id() ->
    [?KEY_ID].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec revision(doc()) -> kz_term:api_binary().
revision(JObj) ->
    kz_json:get_first_defined([?KEY_REV, <<"rev">>], JObj).

-spec revision_id(doc() | kz_term:api_ne_binary()) -> non_neg_integer().
revision_id('undefined') -> 0;
revision_id(<<Rev/binary>>) ->
    [Id, _] = binary:split(Rev, <<"-">>),
    kz_term:to_integer(Id);
revision_id(JObj) ->
    revision_id(revision(JObj)).

-spec path_revision() -> kz_json:path().
path_revision() ->
    [?KEY_REV].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_revision(doc(), kz_term:api_binary()) -> doc().
set_revision(JObj, Rev) ->
    kz_json:set_value(?KEY_REV, Rev, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_revision(doc()) -> doc().
delete_revision(JObj) ->
    kz_json:delete_key(?KEY_REV, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type(doc()) -> kz_term:api_ne_binary().
type(JObj) ->
    type(JObj, 'undefined').

-spec type(doc(), Default) -> kz_term:ne_binary() | Default.
type(JObj, Default) ->
    try kz_json:get_ne_binary_value(?KEY_PVT_TYPE, JObj, Default)
    catch 'error':'badarg' ->
            kz_util:log_stacktrace("~s:type(~s)", [?MODULE, kz_json:encode(JObj)]),
            Default
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_type(doc(), kz_term:ne_binary()) -> doc().
set_type(JObj, Type) ->
    kz_json:set_value(?KEY_PVT_TYPE, Type, JObj).

-spec path_type() -> kz_json:path().
path_type() ->
    [?KEY_PVT_TYPE].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(doc()) -> kz_term:api_ne_binary().
account_id(JObj) ->
    account_id(JObj, 'undefined').

-spec account_id(doc(), Default) -> kz_term:ne_binary() | Default.
account_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_ACCOUNT_ID, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(doc(), kz_term:ne_binary()) -> doc().
set_account_id(JObj, AccountId) ->
    kz_json:set_value(?KEY_ACCOUNT_ID, AccountId, JObj).

-spec path_account_id() -> kz_json:path().
path_account_id() ->
    [?KEY_ACCOUNT_ID].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_db(doc()) -> kz_term:api_ne_binary().
account_db(JObj) ->
    account_db(JObj, 'undefined').

-spec account_db(doc(), Default) -> kz_term:ne_binary() | Default.
account_db(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_ACCOUNT_DB, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_db(doc(), kz_term:ne_binary()) -> doc().
set_account_db(JObj, AccountDb) ->
    kz_json:set_value(?KEY_ACCOUNT_DB, AccountDb, JObj).

-spec path_account_db() -> kz_json:path().
path_account_db() ->
    [?KEY_ACCOUNT_DB].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec created(doc()) -> kz_time:gregorian_seconds() | 'undefined'.
created(JObj) ->
    created(JObj, 'undefined').

-spec created(doc(), Default) -> kz_time:gregorian_seconds() | Default.
created(JObj, Default) ->
    kz_json:get_integer_value(?KEY_CREATED, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_created(doc(), kz_time:gregorian_seconds()) -> doc().
set_created(JObj, Timestamp) ->
    kz_json:set_value(?KEY_CREATED, Timestamp, JObj).

-spec path_created() -> kz_json:path().
path_created() ->
    [?KEY_CREATED].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec modified(doc()) -> kz_time:gregorian_seconds() | 'undefined'.
modified(JObj) ->
    modified(JObj, 'undefined').

-spec modified(doc(), Default) -> kz_time:gregorian_seconds() | Default.
modified(JObj, Default) ->
    kz_json:get_integer_value(?KEY_MODIFIED, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_modified(doc(), kz_time:gregorian_seconds()) -> doc().
set_modified(JObj, Now) ->
    kz_json:set_value(?KEY_MODIFIED, Now, JObj).

-spec path_modified() -> kz_json:path().
path_modified() ->
    [?KEY_MODIFIED].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec vsn(doc()) -> kz_term:api_ne_binary().
vsn(JObj) ->
    vsn(JObj, 'undefined').

-spec vsn(doc(), Default) -> kz_term:ne_binary() | Default.
vsn(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_VSN, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_vsn(doc(), kz_term:ne_binary()) -> doc().
set_vsn(JObj, VSN) ->
    kz_json:set_value(?KEY_VSN, VSN, JObj).

-spec path_vsn() -> kz_json:path().
path_vsn() ->
    [?KEY_VSN].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec document_hash(doc()) -> kz_term:ne_binary().
document_hash(JObj) ->
    case kz_json:get_ne_binary_value(?KEY_DOCUMENT_HASH, JObj) of
        'undefined' -> calculate_document_hash(JObj);
        Hash -> Hash
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_document_hash(doc(), kz_term:ne_binary()) -> doc().
set_document_hash(JObj, Hash) ->
    kz_json:set_value(?KEY_DOCUMENT_HASH, Hash, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_soft_deleted(doc()) -> boolean().
is_soft_deleted(JObj) ->
    kz_json:is_true(?KEY_SOFT_DELETED, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_soft_deleted(doc(), boolean()) -> doc().
set_soft_deleted(JObj, IsSoftDeleted) ->
    kz_json:set_value(?KEY_SOFT_DELETED, kz_term:is_true(IsSoftDeleted), JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_deleted(doc()) -> boolean().
is_deleted(JObj) ->
    kz_json:is_true(?KEY_DELETED, JObj, 'false').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_deleted(doc()) -> doc().
set_deleted(JObj) ->
    set_deleted(JObj, 'true').

-spec set_deleted(doc(), boolean()) -> doc().
set_deleted(JObj, Bool) when is_boolean(Bool) ->
    kz_json:set_value(?KEY_DELETED, Bool, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachments(doc()) -> kz_term:api_object().
attachments(JObj) ->
    attachments(JObj, 'undefined').

-spec attachments(doc(), Default) -> doc() | Default.
attachments(JObj, Default) ->
    A1 = kz_json:get_json_value(?KEY_ATTACHMENTS, JObj, kz_json:new()),
    A2 = kz_json:get_json_value(?KEY_EXTERNAL_ATTACHMENTS, JObj, kz_json:new()),
    case kz_json:merge_jobjs(A1, A2) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

-spec path_attachments() -> kz_json:path().
path_attachments() ->
    [?KEY_ATTACHMENTS].

-spec path_external_attachments() -> kz_json:path().
path_external_attachments() ->
    [?KEY_EXTERNAL_ATTACHMENTS].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec stub_attachments(doc()) -> kz_term:api_object().
stub_attachments(JObj) ->
    stub_attachments(JObj, 'undefined').

-spec stub_attachments(doc(), Default) -> doc() | Default.
stub_attachments(JObj, Default) ->
    case kz_json:get_json_value(?KEY_ATTACHMENTS, JObj, kz_json:new()) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec external_attachments(doc()) -> kz_term:api_object().
external_attachments(JObj) ->
    external_attachments(JObj, 'undefined').

-spec external_attachments(doc(), Default) -> doc() | Default.
external_attachments(JObj, Default) ->
    case kz_json:get_json_value(?KEY_EXTERNAL_ATTACHMENTS, JObj, kz_json:new()) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_names(doc()) -> kz_term:ne_binaries() | [].
attachment_names(JObj) ->
    kz_json:get_keys(attachments(JObj, kz_json:new())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_attachments(doc()) -> doc().
delete_attachments(JObj) ->
    maybe_remove_attachments(JObj, attachments(JObj)).

%%------------------------------------------------------------------------------
%% @doc Gets a random attachment from JObj (no order is imposed!).
%% @end
%%------------------------------------------------------------------------------
-spec attachment(doc()) -> kz_term:api_object().
attachment(JObj) ->
    case kz_json:get_values(attachments(JObj, kz_json:new())) of
        {[], []} -> 'undefined';
        {[Attachment|_], [AttachmentName|_]} ->
            kz_json:from_list([{AttachmentName, Attachment}])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment(doc(), kz_json:key()) -> kz_term:api_object().
attachment(JObj, AName) ->
    attachment(JObj, AName, 'undefined').

-spec attachment(doc(), kz_json:key(), Default) -> doc() | Default.
attachment(JObj, AName, Default) ->
    kz_json:get_json_value(AName, attachments(JObj, kz_json:new()), Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_revision(doc()) -> pos_integer().
attachment_revision(AttachmentJObj) ->
    [Values] = kz_json:values(AttachmentJObj),
    kz_json:get_integer_value(?ATTACHMENT_PROPERTY_REVISION, Values).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_length(doc(), kz_term:ne_binary()) -> kz_term:api_integer().
attachment_length(JObj, AName) ->
    attachment_length(JObj, AName, 'undefined').

-spec attachment_length(doc(), kz_term:ne_binary(), Default) -> non_neg_integer() | Default.
attachment_length(JObj, AName, Default) ->
    attachment_property(JObj, AName, <<"length">>, Default, fun kz_json:get_integer_value/3).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_content_type(doc()) -> kz_term:api_ne_binary().
attachment_content_type(JObj) ->
    case kz_json:get_values(attachments(JObj, kz_json:new())) of
        {[], []} -> 'undefined';
        {[_Attachment|_], [AName|_]} ->
            attachment_content_type(JObj, AName)
    end.

-spec attachment_content_type(doc(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
attachment_content_type(JObj, AName) ->
    attachment_content_type(JObj, AName, 'undefined').

-spec attachment_content_type(doc(), kz_term:ne_binary(), Default) -> Default | kz_term:ne_binary().
attachment_content_type(JObj, AName, Default) ->
    attachment_property(JObj, AName, <<"content_type">>, Default, fun kz_json:get_ne_binary_value/3).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_property(doc(), kz_term:ne_binary(), kz_json:get_key()) ->
                                 kz_json:api_json_term().
attachment_property(JObj, AName, Key) ->
    attachment_property(JObj, AName, Key, 'undefined').

-spec attachment_property(doc(), kz_term:ne_binary(), kz_json:get_key(), Default) ->
                                 Default | kz_json:json_term().
attachment_property(JObj, AName, Key, Default) ->
    attachment_property(JObj, AName, Key, Default, fun kz_json:get_value/3).

-spec attachment_property(doc(), kz_term:ne_binary(), kz_json:get_key(), Default, fun((kz_json:get_key(), doc(), Default) -> Default | kz_json:json_term())) ->
                                 Default | kz_json:json_term().
attachment_property(JObj, AName, Key, Default, Get) when is_function(Get, 3) ->
    Get(Key, attachment(JObj, AName, kz_json:new()), Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_attachment(doc(), kz_term:ne_binary()) -> doc().
delete_attachment(JObj, AName) ->
    maybe_remove_attachment(JObj, AName, attachment(JObj, AName)).

%%------------------------------------------------------------------------------
%% @doc This function is used to update the private timestamps, and db
%% parameters on all crossbar documents.
%% @end
%%------------------------------------------------------------------------------
-spec update_pvt_parameters(doc(), kz_term:api_binary()) ->
                                   doc().
update_pvt_parameters(JObj0, DBName) ->
    update_pvt_parameters(JObj0, DBName, []).

-spec update_pvt_parameters(doc(), kz_term:api_ne_binary(), kz_term:proplist()) ->
                                   doc().
update_pvt_parameters(JObj, DbName, Options) ->
    Opts = props:insert_value('now', kz_time:now_s(), Options),
    Updates = get_pvt_updates(JObj, DbName, Opts),
    kz_json:set_values(Updates, JObj).

-spec get_pvt_updates(kz_json:object(), kz_term:api_ne_binary(), kz_term:proplist()) ->
                             kz_term:proplist().
get_pvt_updates(JObj, DbName, Options) ->
    lists:foldl(fun(Fun, Acc) -> Fun(Acc, JObj, DbName, Options) end, [], ?PVT_FUNS).

-spec add_pvt_vsn(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_vsn(Acc, _JObj, _, Options) ->
    case props:get_value('crossbar_doc_vsn', Options) of
        'undefined' -> Acc;
        Vsn -> [{?KEY_VSN, Vsn} | Acc]
    end.

-spec add_pvt_account_db(kz_term:proplist(), doc(), kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_account_db(Acc, _JObj, 'undefined', Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> Acc;
        Db -> [{?KEY_ACCOUNT_DB, Db} | Acc]
    end;
add_pvt_account_db(Acc, _JObj, DBName, Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' ->
            [{?KEY_ACCOUNT_DB, DBName} | Acc];
        Db ->
            [{?KEY_ACCOUNT_DB, Db} | Acc]
    end.

-spec add_pvt_account_id(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_account_id(Acc, _JObj, 'undefined', Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> Acc;
        Id -> [{?KEY_ACCOUNT_ID, Id} | Acc]
    end;
add_pvt_account_id(Acc, _JObj, DBName, Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' ->
            [{?KEY_ACCOUNT_ID, kz_util:format_account_id(DBName, 'raw')} | Acc];
        Id ->
            [{?KEY_ACCOUNT_ID, Id} | Acc]
    end.

-spec add_pvt_type(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_type(Acc, _JObj, _, Options) ->
    case props:get_value('type', Options) of
        'undefined' -> Acc;
        Type -> [{?KEY_PVT_TYPE, Type} | Acc]
    end.

-spec add_pvt_node(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_node(Acc, _JObj, _, Options) ->
    case props:get_value('node', Options) of
        'undefined' ->
            [{?KEY_NODE, kz_term:to_binary(node())} | Acc];
        Node ->
            [{?KEY_NODE, kz_term:to_binary(Node)} | Acc]
    end.

-spec add_pvt_created(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_created(Acc, JObj, _, Opts) ->
    case kz_json:get_ne_binary_value(?KEY_REV, JObj) of
        'undefined' ->
            [{?KEY_CREATED, props:get_value('now', Opts, kz_time:now_s())} | Acc];
        _Rev -> Acc
    end.

-spec add_pvt_modified(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_modified(Acc, _JObj, _, Opts) ->
    [{?KEY_MODIFIED, props:get_value('now', Opts, kz_time:now_s())} | Acc].

-spec add_id(kz_term:proplist(), doc(), any(), kz_term:proplist()) -> kz_term:proplist().
add_id(Acc, _JObj, _, Opts) ->
    case props:get_value('id', Opts) of
        'undefined' -> Acc;
        Id -> [{?KEY_ID, Id} | Acc]
    end.

-spec add_pvt_document_hash(kz_term:proplist(), doc(), kz_term:api_ne_binary(), kz_term:proplist()) -> kz_term:proplist().
add_pvt_document_hash(Acc, JObj, _, _) ->
    Hash = calculate_document_hash(JObj),
    [{?KEY_DOCUMENT_HASH, Hash} | Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_pvt_modified(doc()) -> doc().
update_pvt_modified(JObj) ->
    kz_json:set_value(?KEY_MODIFIED, kz_time:now_s(), JObj).

%%------------------------------------------------------------------------------
%% @doc This function will filter any private fields out of the provided
%% JSON proplist.
%% @end
%%------------------------------------------------------------------------------
-spec public_fields(doc() | kz_json:objects()) ->
                           doc() | kz_json:objects().
public_fields(Thing) ->
    public_fields(Thing, 'true').

-spec public_fields(doc() | kz_json:objects(), boolean()) ->
                           doc() | kz_json:objects().
public_fields(JObjs, IncludeId) when is_list(JObjs) ->
    [public_fields(J, IncludeId) || J <- JObjs];
public_fields(JObj, 'true') ->
    kz_json:set_value(<<"id">>, id(JObj, 'null'), filter_public_fields(JObj));
public_fields(JObj, 'false') ->
    filter_public_fields(JObj).

-spec filter_public_fields(doc()) -> doc().
filter_public_fields(JObj) ->
    kz_json:filter(fun({K, _}) -> not is_private_key(K) end, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_public_keys(doc()) -> kz_json:keys().
get_public_keys(JObj) ->
    [Key || Key <- kz_json:get_keys(JObj),
            not is_private_key(Key)
    ].

%%------------------------------------------------------------------------------
%% @doc This function will filter any public fields out of the provided
%% JSON proplist.
%% @end
%%------------------------------------------------------------------------------
-spec private_fields(doc() | kz_json:objects()) ->
                            doc() | kz_json:objects().
private_fields(JObjs) when is_list(JObjs) ->
    [private_fields(JObj) || JObj <- JObjs];
private_fields(JObj) ->
    kz_json:filter(fun({K, _}) -> is_private_key(K) end, JObj).

%%------------------------------------------------------------------------------
%% @doc This function will return a boolean, `true' if the provided key is
%% considered private; otherwise `false'.
%% @end
%%------------------------------------------------------------------------------
-spec is_private_key(binary()) -> boolean().
is_private_key(<<"_", _/binary>>) -> 'true';
is_private_key(<<"pvt_", _/binary>>) -> 'true';
is_private_key(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec leak_private_fields(doc()) -> doc().
leak_private_fields(JObj) ->
    kz_json:foldl(fun leak_private_field/3, JObj, JObj).

leak_private_field(<<"_read_only">>, _Value, JObj) -> JObj;
leak_private_field(Key, Value, JObj) ->
    case is_private_key(Key) of
        'false' -> JObj;
        'true' ->
            kz_json:set_value([<<"_read_only">>, remove_pvt(Key)]
                             ,Value
                             ,kz_json:delete_key(Key, JObj)
                             )
    end.

remove_pvt(<<"_", Key/binary>>) -> Key;
remove_pvt(<<"pvt_", Key/binary>>) -> Key;
remove_pvt(Key) -> Key.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_document_hash(doc()) -> kz_term:ne_binary().
calculate_document_hash(JObj) ->
    PublicJObj = public_fields(JObj),
    Attachments = kz_json:get_json_value(?KEY_ATTACHMENTS, JObj),
    Props = [{<<"public">>, PublicJObj}
            ,{<<"attachments">>, Attachments}
            ],
    kz_binary:md5(kz_json:encode(kz_json:from_list(Props))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_remove_attachments(doc()) -> {boolean(), doc()}.
maybe_remove_attachments(JObj) ->
    case attachments(JObj) of
        'undefined' -> {'false', JObj};
        _Attachments -> {'true', kz_json:delete_keys(?KEYS_ATTACHMENTS, JObj)}
    end.

-spec maybe_remove_attachments(doc(), kz_term:api_object()) -> doc().
maybe_remove_attachments(JObj, 'undefined') -> JObj;
maybe_remove_attachments(JObj, _Attachments) ->
    kz_json:delete_keys(?KEYS_ATTACHMENTS, JObj).

-spec maybe_remove_attachment(doc(), kz_term:ne_binary(), kz_term:api_object()) -> doc().
maybe_remove_attachment(JObj, _AName, 'undefined') -> JObj;
maybe_remove_attachment(JObj, AName, _AMeta) ->
    kz_json:delete_keys(?KEYS_ATTACHMENTS(AName), JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec latest_attachment_id(doc()) -> kz_term:ne_binary() | 'undefined'.
latest_attachment_id(Doc) ->
    case attachments(Doc) of
        'undefined' -> 'undefined';
        JObj ->
            JObjs = [kz_json:from_list([KV]) || KV <- kz_json:to_proplist(JObj)],
            Latest = lists:last(lists:sort(fun compare_attachments/2, JObjs)),
            [Name] = kz_json:get_keys(Latest),
            Name
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec compare_attachments(doc(), doc()) -> boolean().
compare_attachments(AttachmentJObjA, AttachmentJObjB) ->
    attachment_revision(AttachmentJObjA) =< attachment_revision(AttachmentJObjB).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> doc().
setters(Routines) ->
    setters(kz_json:new(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(doc(), setter_funs()) -> doc().
setters(JObj, Routines) ->
    lists:foldl(fun({Setter, Value}, J) ->
                        Setter(J, Value)
                end
               ,JObj
               ,Routines
               ).
