%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Utilities for manipulating Kazoo/Whistle documents
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_doc).

-include("wh_couch.hrl").

-include_lib("whistle/include/wh_types.hrl"). % get the whistle types

-export([update_pvt_parameters/2, update_pvt_parameters/3
         ,public_fields/1
         ,private_fields/1
         ,attachments/1, attachments/2
         ,attachment_names/1
         ,attachment/1, attachment/2, attachment/3

         ,attachment_length/2, attachment_content_type/2
         ,attachment_property/3
         ,delete_attachments/1, delete_attachment/2
         ,maybe_remove_attachments/1, maybe_remove_attachments/2
         ,type/1, type/2, set_type/2
         ,id/1, id/2, set_id/2
         ,revision/1, set_revision/2, delete_revision/1
         ,created/1, set_created/2
         ,modified/1, set_modified/2
         ,vsn/1, vsn/2, set_vsn/2
         ,set_soft_deleted/2, is_soft_deleted/1

         ,account_id/1, account_id/2, set_account_id/2
         ,account_db/1, account_db/2, set_account_db/2
        ]).

-export([update_pvt_modified/1]).

-define(PVT_FUNS, [fun add_pvt_vsn/3
                   ,fun add_pvt_account_id/3
                   ,fun add_pvt_account_db/3
                   ,fun add_pvt_created/3
                   ,fun add_pvt_modified/3
                   ,fun add_pvt_type/3
                   ,fun add_pvt_node/3
                   ,fun add_id/3
                  ]).

-define(KEY_ID, <<"_id">>).
-define(KEY_REV, <<"_rev">>).
-define(KEY_ATTACHMENTS, <<"_attachments">>).
-define(KEY_PVT_TYPE, <<"pvt_type">>).
-define(KEY_ACCOUNT_ID, <<"pvt_account_id">>).
-define(KEY_ACCOUNT_DB, <<"pvt_account_db">>).
-define(KEY_CREATED, <<"pvt_created">>).
-define(KEY_MODIFIED, <<"pvt_modified">>).
-define(KEY_SOFT_DELETED, <<"pvt_deleted">>).
-define(KEY_VSN, <<"pvt_vsn">>).
-define(KEY_NODE, <<"pvt_node">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters(wh_json:object(), api_binary()) ->
                                   wh_json:object().
-spec update_pvt_parameters(wh_json:object(), api_binary(), wh_proplist()) ->
                                   wh_json:object().
update_pvt_parameters(JObj0, DBName) ->
    update_pvt_parameters(JObj0, DBName, []).
update_pvt_parameters(JObj0, DBName, Options) ->
    Opts = props:insert_value('now', wh_util:current_tstamp(), Options),
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DBName, Opts) end, JObj0, ?PVT_FUNS).

-spec add_pvt_vsn(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_vsn(JObj, _, Options) ->
    case props:get_value('crossbar_doc_vsn', Options) of
        'undefined' -> JObj;
        Vsn -> wh_json:set_value(?KEY_VSN, Vsn, JObj)
    end.

-spec add_pvt_account_db(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_account_db(JObj, 'undefined', Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> JObj;
        Db -> wh_json:set_value(?KEY_ACCOUNT_DB, Db, JObj)
    end;
add_pvt_account_db(JObj, DBName, Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> wh_json:set_value(?KEY_ACCOUNT_DB, DBName, JObj);
        Db -> wh_json:set_value(?KEY_ACCOUNT_DB, Db, JObj)
    end.

-spec add_pvt_account_id(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_account_id(JObj, 'undefined', Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> JObj;
        Id -> wh_json:set_value(?KEY_ACCOUNT_ID, Id, JObj)
    end;
add_pvt_account_id(JObj, DBName, Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> wh_json:set_value(?KEY_ACCOUNT_ID, wh_util:format_account_id(DBName, 'raw'), JObj);
        Id -> wh_json:set_value(?KEY_ACCOUNT_ID, Id, JObj)
    end.

-spec add_pvt_type(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_type(JObj, _, Options) ->
    case props:get_value('type', Options) of
        'undefined' -> JObj;
        Type -> wh_json:set_value(?KEY_PVT_TYPE, Type, JObj)
    end.

-spec add_pvt_node(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_node(JObj, _, Options) ->
    case props:get_value('node', Options) of
        'undefined' -> wh_json:set_value(?KEY_NODE, wh_util:to_binary(node()), JObj);
        Node -> wh_json:set_value(?KEY_NODE, wh_util:to_binary(Node), JObj)
    end.

-spec add_pvt_created(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_created(JObj, _, Opts) ->
    case wh_json:get_value(?KEY_REV, JObj) of
        'undefined' ->
            wh_json:set_value(?KEY_CREATED
                              ,props:get_value('now', Opts, wh_util:current_tstamp())
                              ,JObj
                             );
        _ -> JObj
    end.

-spec update_pvt_modified(wh_json:object()) -> wh_json:object().
update_pvt_modified(JObj) ->
    add_pvt_modified(JObj, 'undefined', [{'now', wh_util:current_tstamp()}]).

-spec set_modified(wh_json:object(), gregorian_seconds()) -> wh_json:object().
set_modified(JObj, Now) ->
    wh_json:set_value(?KEY_MODIFIED, Now, JObj).

-spec add_pvt_modified(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
add_pvt_modified(JObj, _, Opts) ->
    wh_json:set_value(?KEY_MODIFIED, props:get_value('now', Opts), JObj).

-spec add_id(wh_json:object(), _, wh_proplist()) -> wh_json:object().
add_id(JObj, _, Opts) ->
    case props:get_value('id', Opts) of
        'undefined' -> JObj;
        Id -> set_id(JObj, Id)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields(wh_json:object() | wh_json:objects()) ->
                           wh_json:object() | wh_json:objects().
public_fields(JObjs) when is_list(JObjs) -> [public_fields(J) || J <- JObjs];
public_fields(JObj) -> wh_json:filter(fun({K, _}) -> not is_private_key(K) end, JObj).

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
-spec private_fields(wh_json:object() | wh_json:objects()) ->
                            wh_json:object() | wh_json:objects().
private_fields(JObjs) when is_list(JObjs) -> [public_fields(JObj) || JObj <- JObjs];
private_fields(JObj) -> wh_json:filter(fun({K, _}) -> is_private_key(K) end, JObj).

-spec attachments(wh_json:object()) -> api_object().
-spec attachments(wh_json:object(), Default) -> wh_json:object() | Default.
attachments(JObj) ->
    attachments(JObj, 'undefined').
attachments(JObj, Default) ->
    wh_json:get_value(?KEY_ATTACHMENTS, JObj, Default).

-spec attachment_names(wh_json:object()) -> ne_binaries().
attachment_names(JObj) ->
    wh_json:get_keys(attachments(JObj, wh_json:new())).

-spec attachment(wh_json:object()) -> api_object().
-spec attachment(wh_json:object(), wh_json:key()) -> api_object().
-spec attachment(wh_json:object(), wh_json:key(), Default) -> wh_json:object() | Default.
attachment(JObj) ->
    case wh_json:get_values(attachments(JObj, wh_json:new())) of
        {[], []} -> 'undefined';
        {[Attachment|_], [AttachmentName|_]} ->
                wh_json:from_list([{AttachmentName, Attachment}])
        end.

attachment(JObj, AName) ->
    attachment(JObj, AName, 'undefined').
attachment(JObj, AName, Default) ->
    wh_json:get_value(AName, attachments(JObj, wh_json:new()), Default).

-spec attachment_length(wh_json:object(), ne_binary()) -> api_integer().
attachment_length(JObj, AName) ->
    attachment_property(JObj, AName, <<"length">>).

-spec attachment_content_type(wh_json:object(), ne_binary()) -> api_binary().
attachment_content_type(JObj, AName) ->
    attachment_property(JObj, AName, <<"content_type">>).

-spec attachment_property(wh_json:object(), ne_binary(), wh_json:key()) -> wh_json:json_term().
attachment_property(JObj, AName, Key) ->
    wh_json:get_value(Key, attachment(JObj, AName, wh_json:new())).

-spec delete_attachments(wh_json:object()) -> wh_json:object().
delete_attachments(JObj) ->
    maybe_remove_attachments(JObj, attachments(JObj)).

delete_attachment(JObj, AName) ->
    maybe_remove_attachment(JObj, AName, attachment(JObj, AName)).

-spec maybe_remove_attachments(wh_json:object()) -> {boolean(), wh_json:object()}.
maybe_remove_attachments(JObj) ->
    case attachments(JObj) of
        'undefined' -> {'false', JObj};
        _Attachments -> {'true', wh_json:delete_key(?KEY_ATTACHMENTS, JObj)}
    end.

-spec maybe_remove_attachments(wh_json:object(), api_object()) -> wh_json:object().
maybe_remove_attachments(JObj, 'undefined') -> JObj;
maybe_remove_attachments(JObj, _Attachments) ->
    wh_json:delete_key(?KEY_ATTACHMENTS, JObj).

-spec maybe_remove_attachment(wh_json:object(), ne_binary(), api_object()) -> wh_json:object().
maybe_remove_attachment(JObj, _AName, 'undefined') -> JObj;
maybe_remove_attachment(JObj, AName, _AMeta) ->
    wh_json:delete_key([?KEY_ATTACHMENTS, AName], JObj).

-spec revision(wh_json:object()) -> api_binary().
revision(JObj) ->
    wh_json:get_first_defined([?KEY_REV, <<"rev">>], JObj).

-spec set_revision(wh_json:object(), api_binary()) -> wh_json:object().
set_revision(JObj, Rev) ->
    wh_json:set_value(?KEY_REV, Rev, JObj).

-spec delete_revision(wh_json:object()) -> wh_json:object().
delete_revision(JObj) ->
    wh_json:delete_key(?KEY_REV, JObj).

-spec id(wh_json:object()) -> api_binary().
-spec id(wh_json:object(), Default) -> ne_binary() | Default.
id(JObj) ->
    id(JObj, 'undefined').

id(JObj, Default) ->
    wh_json:get_first_defined([?KEY_ID, <<"id">>], JObj, Default).

-spec set_id(wh_json:object(), ne_binary()) -> wh_json:object().
set_id(JObj, Id) ->
    wh_json:set_value(?KEY_ID, Id, JObj).

-spec type(wh_json:object()) -> api_binary().
-spec type(wh_json:object(), Default) -> ne_binary() | Default.
type(JObj) ->
    type(JObj, 'undefined').
type(JObj, Default) ->
    wh_json:get_value(?KEY_PVT_TYPE, JObj, Default).

-spec set_type(wh_json:object(), ne_binary()) -> wh_json:object().
set_type(JObj, Type) ->
    wh_json:set_value(?KEY_PVT_TYPE, Type, JObj).

-spec set_soft_deleted(wh_json:object(), boolean()) -> wh_json:object().
set_soft_deleted(JObj, IsSoftDeleted) ->
    wh_json:set_value(?KEY_SOFT_DELETED, wh_util:is_true(IsSoftDeleted), JObj).

-spec is_soft_deleted(wh_json:object()) -> boolean().
is_soft_deleted(JObj) ->
    wh_json:is_true(?KEY_SOFT_DELETED, JObj).

-spec created(wh_json:object()) -> api_integer().
created(JObj) ->
    wh_json:get_integer_value(?KEY_CREATED, JObj).

-spec set_created(wh_json:object(), pos_integer()) -> wh_json:object().
set_created(JObj, Timestamp) ->
    wh_json:set_value(?KEY_CREATED, Timestamp, JObj).

-spec modified(wh_json:object()) -> api_integer().
modified(JObj) ->
    wh_json:get_integer_value(?KEY_MODIFIED, JObj).

-spec account_id(wh_json:object()) -> api_binary().
-spec account_id(wh_json:object(), Default) -> ne_binary() | Default.
account_id(JObj) ->
    account_id(JObj, 'undefined').
account_id(JObj, Default) ->
    wh_json:get_value(?KEY_ACCOUNT_ID, JObj, Default).

-spec set_account_id(wh_json:object(), ne_binary()) -> wh_json:object().
set_account_id(JObj, AccountId) ->
    wh_json:set_value(?KEY_ACCOUNT_ID, AccountId, JObj).

-spec account_db(wh_json:object()) -> api_binary().
-spec account_db(wh_json:object(), Default) -> ne_binary() | Default.
account_db(JObj) ->
    account_db(JObj, 'undefined').
account_db(JObj, Default) ->
    wh_json:get_value(?KEY_ACCOUNT_DB, JObj, Default).

-spec set_account_db(wh_json:object(), ne_binary()) -> wh_json:object().
set_account_db(JObj, AccountDb) ->
    wh_json:set_value(?KEY_ACCOUNT_DB, AccountDb, JObj).

-spec vsn(wh_json:object()) -> api_binary().
-spec vsn(wh_json:object(), Default) -> ne_binary() | Default.
vsn(JObj) ->
    vsn(JObj, 'undefined').
vsn(JObj, Default) ->
    wh_json:get_value(?KEY_VSN, JObj, Default).

-spec set_vsn(wh_json:object(), ne_binary()) -> wh_json:object().
set_vsn(JObj, VSN) ->
    wh_json:set_value(?KEY_VSN, VSN, JObj).
