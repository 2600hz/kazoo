%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Utilities for manipulating Kazoo/Kazoo documents
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_doc).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export([update_pvt_parameters/2, update_pvt_parameters/3
        ,public_fields/1, public_fields/2, get_public_keys/1
        ,private_fields/1, is_private_key/1
        ,leak_private_fields/1

        ,attachments/1, attachments/2
        ,stub_attachments/1, stub_attachments/2
        ,external_attachments/1, external_attachments/2
        ,attachment_names/1
        ,attachment/1, attachment/2, attachment/3
        ,latest_attachment_id/1

        ,attachment_revision/1
        ,compare_attachments/2

        ,attachment_length/2, attachment_length/3
        ,attachment_content_type/1, attachment_content_type/2, attachment_content_type/3
        ,attachment_property/3, attachment_property/4
        ,delete_attachments/1, delete_attachment/2
        ,maybe_remove_attachments/1, maybe_remove_attachments/2

        ,type/1, type/2, set_type/2
        ,id/1, id/2, set_id/2
        ,revision/1, set_revision/2, delete_revision/1
        ,created/1, created/2, set_created/2
        ,modified/1, modified/2, set_modified/2
        ,vsn/1, vsn/2, set_vsn/2
        ,document_hash/1, set_document_hash/2
        ,calculate_document_hash/1
        ,set_soft_deleted/2, is_soft_deleted/1
        ,set_deleted/1, set_deleted/2, is_deleted/1

        ,account_id/1, account_id/2, set_account_id/2
        ,account_db/1, account_db/2, set_account_db/2
        ]).

-export([update_pvt_modified/1]).

-ifdef(TEST).
-export([remove_pvt/1]).
-endif.

-define(PVT_FUNS, [fun add_pvt_vsn/3
                  ,fun add_pvt_account_id/3
                  ,fun add_pvt_account_db/3
                  ,fun add_pvt_created/3
                  ,fun add_pvt_modified/3
                  ,fun add_pvt_type/3
                  ,fun add_pvt_node/3
                  ,fun add_id/3
                  ,fun add_pvt_document_hash/3
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters(kz_json:object(), api_binary()) ->
                                   kz_json:object().
-spec update_pvt_parameters(kz_json:object(), api_binary(), kz_proplist()) ->
                                   kz_json:object().
update_pvt_parameters(JObj0, DBName) ->
    update_pvt_parameters(JObj0, DBName, []).
update_pvt_parameters(JObj0, DBName, Options) ->
    Opts = props:insert_value('now', kz_time:current_tstamp(), Options),
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DBName, Opts) end, JObj0, ?PVT_FUNS).

-spec add_pvt_vsn(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_vsn(JObj, _, Options) ->
    case props:get_value('crossbar_doc_vsn', Options) of
        'undefined' -> JObj;
        Vsn -> kz_json:set_value(?KEY_VSN, Vsn, JObj)
    end.

-spec add_pvt_account_db(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_account_db(JObj, 'undefined', Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> JObj;
        Db -> kz_json:set_value(?KEY_ACCOUNT_DB, Db, JObj)
    end;
add_pvt_account_db(JObj, DBName, Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> kz_json:set_value(?KEY_ACCOUNT_DB, DBName, JObj);
        Db -> kz_json:set_value(?KEY_ACCOUNT_DB, Db, JObj)
    end.

-spec add_pvt_account_id(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_account_id(JObj, 'undefined', Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> JObj;
        Id -> kz_json:set_value(?KEY_ACCOUNT_ID, Id, JObj)
    end;
add_pvt_account_id(JObj, DBName, Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> kz_json:set_value(?KEY_ACCOUNT_ID, kz_util:format_account_id(DBName, 'raw'), JObj);
        Id -> kz_json:set_value(?KEY_ACCOUNT_ID, Id, JObj)
    end.

-spec add_pvt_type(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_type(JObj, _, Options) ->
    case props:get_value('type', Options) of
        'undefined' -> JObj;
        Type -> kz_json:set_value(?KEY_PVT_TYPE, Type, JObj)
    end.

-spec add_pvt_node(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_node(JObj, _, Options) ->
    case props:get_value('node', Options) of
        'undefined' -> kz_json:set_value(?KEY_NODE, kz_term:to_binary(node()), JObj);
        Node -> kz_json:set_value(?KEY_NODE, kz_term:to_binary(Node), JObj)
    end.

-spec add_pvt_created(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_created(JObj, _, Opts) ->
    case kz_json:get_value(?KEY_REV, JObj) of
        'undefined' ->
            kz_json:set_value(?KEY_CREATED
                             ,props:get_value('now', Opts, kz_time:current_tstamp())
                             ,JObj
                             );
        _ -> JObj
    end.

-spec update_pvt_modified(kz_json:object()) -> kz_json:object().
update_pvt_modified(JObj) ->
    add_pvt_modified(JObj, 'undefined', [{'now', kz_time:current_tstamp()}]).

-spec set_modified(kz_json:object(), gregorian_seconds()) -> kz_json:object().
set_modified(JObj, Now) ->
    kz_json:set_value(?KEY_MODIFIED, Now, JObj).

-spec add_pvt_modified(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
add_pvt_modified(JObj, _, Opts) ->
    kz_json:set_value(?KEY_MODIFIED, props:get_value('now', Opts), JObj).

-spec modified(kz_json:object()) -> api_integer().
-spec modified(kz_json:object(), Default) -> integer() | Default.
modified(JObj) ->
    modified(JObj, 'undefined').
modified(JObj, Default) ->
    kz_json:get_integer_value(?KEY_MODIFIED, JObj, Default).

-spec add_id(kz_json:object(), any(), kz_proplist()) -> kz_json:object().
add_id(JObj, _, Opts) ->
    case props:get_value('id', Opts) of
        'undefined' -> JObj;
        Id -> set_id(JObj, Id)
    end.

-spec add_pvt_document_hash(kz_json:object(), any(), kz_proplist()) -> kz_json:object().
add_pvt_document_hash(JObj, _, _) ->
    Hash = calculate_document_hash(JObj),
    set_document_hash(JObj, Hash).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields(kz_json:object() | kz_json:objects()) ->
                           kz_json:object() | kz_json:objects().
public_fields(Thing) ->
    public_fields(Thing, 'true').

-spec public_fields(kz_json:object() | kz_json:objects(), boolean()) ->
                           kz_json:object() | kz_json:objects().
public_fields(JObjs, IncludeId) when is_list(JObjs) ->
    [public_fields(J, IncludeId) || J <- JObjs];
public_fields(JObj, 'true') ->
    kz_json:set_value(<<"id">>, id(JObj, 'null'), filter_public_fields(JObj));
public_fields(JObj, 'false') ->
    filter_public_fields(JObj).

-spec filter_public_fields(kz_json:object()) -> kz_json:object().
filter_public_fields(JObj) ->
    kz_json:filter(fun({K, _}) -> not is_private_key(K) end, JObj).

-spec get_public_keys(kz_json:object()) -> kz_json:keys().
get_public_keys(JObj) ->
    [Key || Key <- kz_json:get_keys(JObj),
            not is_private_key(Key)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec is_private_key(binary()) -> boolean().
is_private_key(<<"_", _/binary>>) -> 'true';
is_private_key(<<"pvt_", _/binary>>) -> 'true';
is_private_key(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec private_fields(kz_json:object() | kz_json:objects()) ->
                            kz_json:object() | kz_json:objects().
private_fields(JObjs) when is_list(JObjs) ->
    [private_fields(JObj) || JObj <- JObjs];
private_fields(JObj) ->
    kz_json:filter(fun({K, _}) -> is_private_key(K) end, JObj).

-spec leak_private_fields(kz_json:object()) -> kz_json:object().
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

-spec attachments(kz_json:object()) -> api_object().
-spec attachments(kz_json:object(), Default) -> kz_json:object() | Default.
attachments(JObj) ->
    attachments(JObj, 'undefined').
attachments(JObj, Default) ->
    A1 = kz_json:get_value(?KEY_ATTACHMENTS, JObj, kz_json:new()),
    A2 = kz_json:get_value(?KEY_EXTERNAL_ATTACHMENTS, JObj, kz_json:new()),
    case kz_json:merge_jobjs(A1, A2) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

-spec stub_attachments(kz_json:object()) -> api_object().
-spec stub_attachments(kz_json:object(), Default) -> kz_json:object() | Default.
stub_attachments(JObj) ->
    stub_attachments(JObj, 'undefined').
stub_attachments(JObj, Default) ->
    case kz_json:get_value(?KEY_ATTACHMENTS, JObj, kz_json:new()) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

-spec external_attachments(kz_json:object()) -> api_object().
-spec external_attachments(kz_json:object(), Default) -> kz_json:object() | Default.
external_attachments(JObj) ->
    external_attachments(JObj, 'undefined').
external_attachments(JObj, Default) ->
    case kz_json:get_value(?KEY_EXTERNAL_ATTACHMENTS, JObj, kz_json:new()) of
        ?EMPTY_JSON_OBJECT -> Default;
        A3 -> A3
    end.

-spec attachment_names(kz_json:object()) -> ne_binaries() | [].
attachment_names(JObj) ->
    kz_json:get_keys(attachments(JObj, kz_json:new())).

-spec attachment_revision(kz_json:object()) -> pos_integer().
attachment_revision(AttachmentJObj) ->
    [Values] = kz_json:values(AttachmentJObj),
    kz_json:get_value(?ATTACHMENT_PROPERTY_REVISION, Values).

-spec compare_attachments(kz_json:object(), kz_json:object()) -> boolean().
compare_attachments(AttachmentJObjA, AttachmentJObjB) ->
    attachment_revision(AttachmentJObjA) =< attachment_revision(AttachmentJObjB).

-spec latest_attachment_id(kz_json:object()) -> ne_binary() | 'undefined'.
latest_attachment_id(Doc) ->
    case attachments(Doc) of
        'undefined' -> 'undefined';
        JObj ->
            JObjs = [kz_json:from_list([KV]) || KV <- kz_json:to_proplist(JObj)],
            Latest = lists:last(lists:sort(fun compare_attachments/2, JObjs)),
            [Name] = kz_json:get_keys(Latest),
            Name
    end.

-spec attachment(kz_json:object()) -> api_object().
-spec attachment(kz_json:object(), kz_json:path()) -> api_object().
-spec attachment(kz_json:object(), kz_json:path(), Default) -> kz_json:object() | Default.
%% @public
%% @doc Gets a random attachment from JObj (no order is imposed!)
attachment(JObj) ->
    case kz_json:get_values(attachments(JObj, kz_json:new())) of
        {[], []} -> 'undefined';
        {[Attachment|_], [AttachmentName|_]} ->
            kz_json:from_list([{AttachmentName, Attachment}])
    end.

attachment(JObj, AName) ->
    attachment(JObj, AName, 'undefined').
attachment(JObj, AName, Default) ->
    kz_json:get_value(AName, attachments(JObj, kz_json:new()), Default).

-spec attachment_length(kz_json:object(), ne_binary()) -> api_integer().
-spec attachment_length(kz_json:object(), ne_binary(), Default) -> non_neg_integer() | Default.
attachment_length(JObj, AName) ->
    attachment_length(JObj, AName, 'undefined').
attachment_length(JObj, AName, Default) ->
    attachment_property(JObj, AName, <<"length">>, Default, fun kz_json:get_integer_value/3).

-spec attachment_content_type(kz_json:object()) -> api_ne_binary().
-spec attachment_content_type(kz_json:object(), ne_binary()) -> api_ne_binary().
-spec attachment_content_type(kz_json:object(), ne_binary(), Default) -> Default | ne_binary().
attachment_content_type(JObj) ->
    case kz_json:get_values(attachments(JObj, kz_json:new())) of
        {[], []} -> 'undefined';
        {[_Attachment|_], [AName|_]} ->
            attachment_content_type(JObj, AName)
    end.
attachment_content_type(JObj, AName) ->
    attachment_content_type(JObj, AName, 'undefined').
attachment_content_type(JObj, AName, Default) ->
    attachment_property(JObj, AName, <<"content_type">>, Default, fun kz_json:get_ne_binary_value/3).

-spec attachment_property(kz_json:object(), ne_binary(), kz_json:path()) ->
                                 kz_json:api_json_term().
-spec attachment_property(kz_json:object(), ne_binary(), kz_json:path(), Default) ->
                                 Default | kz_json:json_term().
-spec attachment_property(kz_json:object(), ne_binary(), kz_json:path(), Default, fun((kz_json:path(), kz_json:object(), Default) -> Default | kz_json:json_term())) ->
                                 Default | kz_json:json_term().
attachment_property(JObj, AName, Key) ->
    attachment_property(JObj, AName, Key, 'undefined').
attachment_property(JObj, AName, Key, Default) ->
    attachment_property(JObj, AName, Key, Default, fun kz_json:get_value/3).
attachment_property(JObj, AName, Key, Default, Get) when is_function(Get, 3) ->
    Get(Key, attachment(JObj, AName, kz_json:new()), Default).

-spec delete_attachments(kz_json:object()) -> kz_json:object().
delete_attachments(JObj) ->
    maybe_remove_attachments(JObj, attachments(JObj)).

-spec delete_attachment(kz_json:object(), ne_binary()) -> kz_json:object().
delete_attachment(JObj, AName) ->
    maybe_remove_attachment(JObj, AName, attachment(JObj, AName)).

-spec maybe_remove_attachments(kz_json:object()) -> {boolean(), kz_json:object()}.
maybe_remove_attachments(JObj) ->
    case attachments(JObj) of
        'undefined' -> {'false', JObj};
        _Attachments -> {'true', kz_json:delete_keys(?KEYS_ATTACHMENTS, JObj)}
    end.

-spec maybe_remove_attachments(kz_json:object(), api_object()) -> kz_json:object().
maybe_remove_attachments(JObj, 'undefined') -> JObj;
maybe_remove_attachments(JObj, _Attachments) ->
    kz_json:delete_keys(?KEYS_ATTACHMENTS, JObj).

-spec maybe_remove_attachment(kz_json:object(), ne_binary(), api_object()) -> kz_json:object().
maybe_remove_attachment(JObj, _AName, 'undefined') -> JObj;
maybe_remove_attachment(JObj, AName, _AMeta) ->
    kz_json:delete_keys(?KEYS_ATTACHMENTS(AName), JObj).

-spec revision(kz_json:object()) -> api_binary().
revision(JObj) ->
    kz_json:get_first_defined([?KEY_REV, <<"rev">>], JObj).

-spec set_revision(kz_json:object(), api_binary()) -> kz_json:object().
set_revision(JObj, Rev) ->
    kz_json:set_value(?KEY_REV, Rev, JObj).

-spec delete_revision(kz_json:object()) -> kz_json:object().
delete_revision(JObj) ->
    kz_json:delete_key(?KEY_REV, JObj).

-spec id(kz_json:object()) -> api_binary().
-spec id(kz_json:object(), Default) -> ne_binary() | Default.
id(JObj) ->
    id(JObj, 'undefined').

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

-spec set_id(kz_json:object(), ne_binary()) -> kz_json:object().
set_id(JObj, Id) ->
    kz_json:set_value(?KEY_ID, Id, JObj).

-spec type(kz_json:object()) -> api_binary().
-spec type(kz_json:object(), Default) -> ne_binary() | Default.
type(JObj) ->
    type(JObj, 'undefined').
type(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_PVT_TYPE, JObj, Default).

-spec set_type(kz_json:object(), ne_binary()) -> kz_json:object().
set_type(JObj, Type) ->
    kz_json:set_value(?KEY_PVT_TYPE, Type, JObj).

-spec set_soft_deleted(kz_json:object(), boolean()) -> kz_json:object().
set_soft_deleted(JObj, IsSoftDeleted) ->
    kz_json:set_value(?KEY_SOFT_DELETED, kz_term:is_true(IsSoftDeleted), JObj).

-spec is_soft_deleted(kz_json:object()) -> boolean().
is_soft_deleted(JObj) ->
    kz_json:is_true(?KEY_SOFT_DELETED, JObj).

-spec set_deleted(kz_json:object()) -> kz_json:object().
-spec set_deleted(kz_json:object(), boolean()) -> kz_json:object().
set_deleted(JObj) ->
    set_deleted(JObj, 'true').
set_deleted(JObj, Bool) when is_boolean(Bool) ->
    kz_json:set_value(?KEY_DELETED, Bool, JObj).

-spec is_deleted(kz_json:object()) -> boolean().
is_deleted(JObj) ->
    kz_json:is_true(?KEY_DELETED, JObj, 'false').

-spec created(kz_json:object()) -> api_integer().
-spec created(kz_json:object(), Default) -> integer() | Default.
created(JObj) ->
    created(JObj, 'undefined').
created(JObj, Default) ->
    kz_json:get_integer_value(?KEY_CREATED, JObj, Default).

-spec set_created(kz_json:object(), pos_integer()) -> kz_json:object().
set_created(JObj, Timestamp) ->
    kz_json:set_value(?KEY_CREATED, Timestamp, JObj).

-spec account_id(kz_json:object()) -> api_binary().
-spec account_id(kz_json:object(), Default) -> ne_binary() | Default.
account_id(JObj) ->
    account_id(JObj, 'undefined').
account_id(JObj, Default) ->
    kz_json:get_value(?KEY_ACCOUNT_ID, JObj, Default).

-spec set_account_id(kz_json:object(), ne_binary()) -> kz_json:object().
set_account_id(JObj, AccountId) ->
    kz_json:set_value(?KEY_ACCOUNT_ID, AccountId, JObj).

-spec account_db(kz_json:object()) -> api_binary().
-spec account_db(kz_json:object(), Default) -> ne_binary() | Default.
account_db(JObj) ->
    account_db(JObj, 'undefined').
account_db(JObj, Default) ->
    kz_json:get_value(?KEY_ACCOUNT_DB, JObj, Default).

-spec set_account_db(kz_json:object(), ne_binary()) -> kz_json:object().
set_account_db(JObj, AccountDb) ->
    kz_json:set_value(?KEY_ACCOUNT_DB, AccountDb, JObj).

-spec vsn(kz_json:object()) -> api_binary().
-spec vsn(kz_json:object(), Default) -> ne_binary() | Default.
vsn(JObj) ->
    vsn(JObj, 'undefined').
vsn(JObj, Default) ->
    kz_json:get_value(?KEY_VSN, JObj, Default).

-spec set_vsn(kz_json:object(), ne_binary()) -> kz_json:object().
set_vsn(JObj, VSN) ->
    kz_json:set_value(?KEY_VSN, VSN, JObj).

-spec document_hash(kz_json:object()) -> ne_binary().
document_hash(JObj) ->
    case kz_json:get_value(?KEY_DOCUMENT_HASH, JObj) of
        'undefined' -> calculate_document_hash(JObj);
        Hash -> Hash
    end.

-spec set_document_hash(kz_json:object(), ne_binary()) -> kz_json:object().
set_document_hash(JObj, Hash) ->
    kz_json:set_value(?KEY_DOCUMENT_HASH, Hash, JObj).

-spec calculate_document_hash(kz_json:object()) -> ne_binary().
calculate_document_hash(JObj) ->
    PublicJObj = public_fields(JObj),
    Attachments = kz_json:get_json_value(?KEY_ATTACHMENTS, JObj),
    Props = [{<<"public">>, PublicJObj}, {<<"attachments">>, Attachments}],
    kz_binary:md5(kz_json:encode(kz_json:from_list(Props))).
