%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(kapps_config).

-include("kazoo_config.hrl").
-include_lib("kazoo/src/kz_json.hrl").

-export([get/2, get/3, get/4
        ,get_all_kvs/1
        ,get_current/2, get_current/3, get_current/4
        ]).
-export([get_node_value/2
        ,get_node_value/3
        ,get_node_value/4
        ]).
-export([get_string/2, get_string/3, get_string/4]).
-export([get_binary/2, get_binary/3, get_binary/4]).
-export([get_json/2, get_json/3, get_json/4]).
-export([get_atom/2, get_atom/3, get_atom/4]).
-export([get_integer/2, get_integer/3, get_integer/4]).
-export([get_float/2, get_float/3, get_float/4]).
-export([get_is_false/2, get_is_false/3, get_is_false/4]).
-export([get_is_true/2, get_is_true/3, get_is_true/4]).
-export([get_non_empty/2, get_non_empty/3, get_non_empty/4]).
-export([get_ne_binary/2, get_ne_binary/3, get_ne_binary/4]).

-export([set/3, set/4, set_default/3, set_node/4
        ,update_default/3, update_default/4
        ]).
-export([lock_db/0, lock_db/1, is_locked/0]).
-export([flush/0, flush/1, flush/2, flush/3]).

-export([migrate/0]).

-type config_category() :: ne_binary() | nonempty_string() | atom().
-type config_key() :: ne_binary() | nonempty_string() | atom() | [config_key(),...].

-type update_option() :: {'node_specific', boolean()} |
                         {'pvt_fields', api_object()}.
-type update_options() :: [update_option()].

-type fetch_ret() :: {'ok', kz_json:object()} |
                     {'error', 'not_found'}.

-define(KEY_DEFAULT, <<"default">>).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a list
%%-----------------------------------------------------------------------------
-spec get_string(config_category(), config_key()) -> api_string().
-spec get_string(config_category(), config_key(), Default) ->
                        nonempty_string() | Default.
-spec get_string(config_category(), config_key(), Default, ne_binary()) ->
                        nonempty_string() | Default.

get_string(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:to_list(Else)
    end.
get_string(Category, Key, Default) ->
    get_string(Category, Key, Default, kz_util:to_binary(node())).
get_string(Category, Key, Default, Node) ->
    kz_util:to_list(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a binary
%%-----------------------------------------------------------------------------
-spec get_binary(config_category(), config_key()) -> api_binary().
-spec get_binary(config_category(), config_key(), Default) -> binary() | Default.
-spec get_binary(config_category(), config_key(), Default, ne_binary()) -> binary() | Default.

get_binary(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:to_binary(Else)
    end.
get_binary(Category, Key, Default) ->
    get_binary(Category, Key, Default, kz_util:to_binary(node())).
get_binary(Category, Key, Default, Node) ->
    kz_util:to_binary(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a json
%%-----------------------------------------------------------------------------
-spec get_json(config_category(), config_key()) ->
                      api_object().
-spec get_json(config_category(), config_key(), Default) ->
                      kz_json:object() | Default.
-spec get_json(config_category(), config_key(), Default, ne_binary()) ->
                      kz_json:object() | Default.

get_json(Category, Key) ->
    V = get(Category, Key),
    as_json_value(V).

-spec as_json_value(any()) -> api_object().
as_json_value('undefined') -> 'undefined';
as_json_value(V) ->
    case kz_json:is_json_object(V) of
        'true' -> V;
        'false' -> 'undefined'
    end.

get_json(Category, Key, Default) ->
    get_json(Category, Key, Default, kz_util:to_binary(node())).
get_json(Category, Key, Default, Node) ->
    V = get(Category, Key, Default, Node),
    case kz_json:is_json_object(V) of
        'true' -> V;
        'false' -> Default
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a atom
%%-----------------------------------------------------------------------------
-spec get_atom(config_category(), config_key()) -> api_atom().
-spec get_atom(config_category(), config_key(), Default) -> atom() | Default.
-spec get_atom(config_category(), config_key(), Default, ne_binary()) -> atom() | Default.

get_atom(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:to_atom(Else, 'true')
    end.
get_atom(Category, Key, Default) ->
    get_atom(Category, Key, Default, kz_util:to_binary(node())).
get_atom(Category, Key, Default, Node) ->
    kz_util:to_atom(get(Category, Key, Default, Node), 'true').

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a integer
%%-----------------------------------------------------------------------------
-spec get_integer(config_category(), config_key()) -> api_integer().
-spec get_integer(config_category(), config_key(), Default) -> integer() | Default.
-spec get_integer(config_category(), config_key(), Default, ne_binary()) -> integer() | Default.

get_integer(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:to_integer(Else)
    end.
get_integer(Category, Key, Default) ->
    get_integer(Category, Key, Default, kz_util:to_binary(node())).
get_integer(Category, Key, Default, Node) ->
    kz_util:to_integer(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a float
%%-----------------------------------------------------------------------------
-spec get_float(config_category(), config_key()) -> api_float().
-spec get_float(config_category(), config_key(), Default) -> float() | Default.
-spec get_float(config_category(), config_key(), Default, ne_binary()) -> float() | Default.

get_float(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:to_float(Else)
    end.
get_float(Category, Key, Default) ->
    get_float(Category, Key, Default, kz_util:to_binary(node())).
get_float(Category, Key, Default, Node) ->
    kz_util:to_float(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a is_false
%%-----------------------------------------------------------------------------
-spec get_is_false(config_category(), config_key()) -> api_boolean().
-spec get_is_false(config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_false(config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.

get_is_false(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:is_false(Else)
    end.
get_is_false(Category, Key, Default) ->
    get_is_false(Category, Key, Default, kz_util:to_binary(node())).
get_is_false(Category, Key, Default, Node) ->
    kz_util:is_false(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a is_true
%%-----------------------------------------------------------------------------
-spec get_is_true(config_category(), config_key()) -> api_boolean().
-spec get_is_true(config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_true(config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.

get_is_true(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> kz_util:is_true(Else)
    end.
get_is_true(Category, Key, Default) ->
    get_is_true(Category, Key, Default, kz_util:to_binary(node())).
get_is_true(Category, Key, Default, Node) ->
    kz_util:is_true(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get a configuration key for a given category and cast it as a is_true
%%-----------------------------------------------------------------------------
-spec get_non_empty(config_category(), config_key()) -> _ | 'undefined'.
-spec get_non_empty(config_category(), config_key(), Default) -> _ | Default.
-spec get_non_empty(config_category(), config_key(), Default, ne_binary()) -> _ | Default.

get_non_empty(Category, Key) ->
    get_non_empty(Category, Key, 'undefined').
get_non_empty(Category, Key, Default) ->
    get_non_empty(Category, Key, Default, kz_util:to_binary(node())).
get_non_empty(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case kz_util:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

-spec get_ne_binary(config_category(), config_key()) -> api_binary().
-spec get_ne_binary(config_category(), config_key(), Default) -> ne_binary() | Default.
-spec get_ne_binary(config_category(), config_key(), Default, ne_binary()) -> ne_binary() | Default.

get_ne_binary(Category, Key) ->
    get_ne_binary(Category, Key, 'undefined').
get_ne_binary(Category, Key, Default) ->
    get_ne_binary(Category, Key, Default, kz_util:to_binary(node())).
get_ne_binary(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case kz_util:is_empty(Value) of
        'true' -> Default;
        'false' -> kz_util:to_binary(Value)
    end.


%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Get a configuration key for a given category but only if its configured
%%  explicitly for the node
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_node_value(config_category(), config_key()) -> any() | 'undefined'.
-spec get_node_value(config_category(), config_key(), Default) -> any() | Default.
-spec get_node_value(config_category(), config_key(), Default, ne_binary() | atom()) -> any() | Default.

get_node_value(Category, Key) ->
    get_node_value(Category, Key, 'undefined').

get_node_value(Category, Key, Default) ->
    get_node_value(Category, Key, Default, node()).

get_node_value(Category, Key, Default, Node) when not is_list(Key) ->
    get_node_value(Category, [kz_util:to_binary(Key)], Default, Node);
get_node_value(Category, Keys, Default, Node) when not is_binary(Category) ->
    get_node_value(kz_util:to_binary(Category), Keys, Default, Node);
get_node_value(Category, Keys, Default, Node) when not is_binary(Node) ->
    get_node_value(Category, Keys, Default, kz_util:to_binary(Node));
get_node_value(Category, Keys, Default, Node) ->
    case get_category(Category) of
        {'ok', JObj} ->
            Node = kz_util:to_binary(node()),
            kz_json:get_value([Node | Keys], JObj);
        {'error', 'not_found'} ->
            lager:debug("missing category ~s ~p: ~p", [Category, Keys, Default]),
            Default
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Get a configuration key for a given category
%%
%% Also, when looking up the key see if there is a value specific to this
%% node but if there is not then use the default value.
%% @end
%%-----------------------------------------------------------------------------
-spec get(config_category(), config_key()) -> any() | 'undefined'.
-spec get(config_category(), config_key(), Default) -> any() | Default.
-spec get(config_category(), config_key(), Default, ne_binary() | atom()) -> any() | Default.

-ifdef(TEST).
get(_, _) -> 'undefined'.
get(_, _, Default) -> Default.
get(_, _, Default, _) -> Default.
get_current(_, _) -> 'undefined'.
get_current(_, _, Default) -> Default.
get_current(_, _, Default, _) -> Default.
-else.

get(Category, Key) ->
    get(Category, Key, 'undefined').

get(Category, Key, Default) ->
    get(Category, Key, Default, node()).

get(Category, Key, Default, 'undefined') ->
    get(Category, Key, Default, ?KEY_DEFAULT);
get(Category, Key, Default, Node) when not is_list(Key) ->
    get(Category, [kz_util:to_binary(Key)], Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Category) ->
    get(kz_util:to_binary(Category), Keys, Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Node) ->
    get(Category, Keys, Default, kz_util:to_binary(Node));
get(Category, Keys, Default, Node) ->
    case get_category(Category) of
        {'ok', JObj} -> get_value(Category, Node, Keys, Default, JObj);
        {'error', 'not_found'} ->
            lager:debug("missing category ~s(default) ~p: ~p", [Category, Keys, Default]),
            _ = set(Category, Keys, Default),
            Default;
        {'error', Error} ->
            lager:debug("error ~p getting  category ~s(default) ~p: ~p", [Error, Category, Keys, Default]),
            Default
    end.

-spec get_current(config_category(), config_key()) -> any() | 'undefined'.
-spec get_current(config_category(), config_key(), Default) -> any() | Default.
-spec get_current(config_category(), config_key(), Default, ne_binary() | atom()) -> any() | Default.

get_current(Category, Key) ->
    get_current(Category, Key, 'undefined').

get_current(Category, Key, Default) ->
    get_current(Category, Key, Default, node()).

get_current(Category, Key, Default, 'undefined') ->
    get_current(Category, Key, Default, ?KEY_DEFAULT);
get_current(Category, Key, Default, Node) when not is_list(Key) ->
    get_current(Category, [kz_util:to_binary(Key)], Default, Node);
get_current(Category, Keys, Default, Node) when not is_binary(Category) ->
    get_current(kz_util:to_binary(Category), Keys, Default, Node);
get_current(Category, Keys, Default, Node) when not is_binary(Node) ->
    get_current(Category, Keys, Default, kz_util:to_binary(Node));
get_current(Category, Keys, Default, Node) ->
    case get_category(Category, 'false') of
        {'ok', JObj} -> get_value(Category, Node, Keys, Default, JObj);
        {'error', 'not_found'} ->
            lager:debug("missing category ~s(default) ~p: ~p", [Category, Keys, Default]),
            _ = set(Category, Keys, Default),
            Default;
        {'error', Error} ->
            lager:debug("error ~p getting  category ~s(default) ~p: ~p", [Error, Category, Keys, Default]),
            Default
    end.

-spec get_value(config_category(), config_key(), config_key(), Default, kz_json:object()) ->
                       Default | any().
get_value(Category, ?KEY_DEFAULT, Keys, Default, JObj) ->
    get_default_value(Category, Keys, Default, JObj);
get_value(Category, Node, Keys, Default, JObj) ->
    case kz_json:get_value([Node | Keys], JObj) of
        'undefined' -> get_zone_value(Category, Node, Keys, Default, JObj);
        Else -> Else
    end.

-spec get_zone_value(config_category(), config_key(), config_key(), Default, kz_json:object()) ->
                            Default | any().
get_zone_value(Category, _Node, Keys, Default, JObj) ->
    Zone = kz_config:zone(),
    case kz_json:get_value([Zone | Keys], JObj) of
        'undefined' -> get_default_value(Category, Keys, Default, JObj);
        Else -> Else
    end.

-spec get_default_value(config_category(), config_key(), Default, kz_json:object()) ->
                               Default | _.
get_default_value(Category, Keys, Default, JObj) ->
    case kz_json:get_value([?KEY_DEFAULT | Keys], JObj) of
        'undefined' ->
            lager:debug("setting default for ~s ~p: ~p", [Category, Keys, Default]),
            _ = set_default(Category, Keys, Default),
            Default;
        Else -> Else
    end.

-endif.

%%-----------------------------------------------------------------------------
%% @public
%% @doc Get all Key-Value pairs for a given category
%%-----------------------------------------------------------------------------
-spec get_all_kvs(ne_binary()) -> kz_proplist().
get_all_kvs(Category) ->
    case get_category(Category) of
        {'error', _} -> [];
        {'ok', JObj} -> get_all_kvs(kz_util:to_binary(node()), JObj)
    end.

-spec get_all_kvs(ne_binary(), kz_json:object()) -> kz_proplist().
get_all_kvs(Node, JObj) ->
    case kz_json:get_value(Node, JObj) of
        'undefined' -> get_all_default_kvs(JObj);
        NodeJObj -> kz_json:to_proplist(NodeJObj)
    end.

-spec get_all_default_kvs(kz_json:object()) -> kz_proplist().
get_all_default_kvs(JObj) ->
    case kz_json:get_value(?KEY_DEFAULT, JObj) of
        'undefined' -> [];
        DefJObj -> kz_json:to_proplist(DefJObj)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc Set the key to the value in the given category but specific to this node
%%-----------------------------------------------------------------------------
-spec set(config_category(), config_key(), any()) ->
                 {'ok', kz_json:object()}.
set(Category, Key, Value) ->
    set(Category, Key, Value, node()).

-spec set(config_category(), config_key(), any(), ne_binary() | atom()) ->
                 {'ok', kz_json:object()}.
set(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, []).

-spec set_default(config_category(), config_key(), any()) ->
                         {'ok', kz_json:object()} | 'ok' |
                         {'error', any()}.
set_default(Category, Key, Value) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, []).

-spec update_default(config_category(), config_key(), kz_json:json_term()) ->
                            {'ok', kz_json:object()} | 'ok' |
                            {'error', any()}.
-spec update_default(config_category(), config_key(), kz_json:json_term(), update_options()) ->
                            {'ok', kz_json:object()} | 'ok' |
                            {'error', any()}.
update_default(Category, Key, Value) ->
    update_default(Category, Key, Value, []).
update_default(Category, Key, Value, Options) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, Options).

-spec set_node(config_category(), config_key(), any(), ne_binary() | atom()) ->
                      {'ok', kz_json:object()}.
set_node(Category, _, _, 'undefined') -> get_category(Category);
set_node(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, [{'node_specific', 'true'}]).

-spec update_category(config_category(), config_key(), any(), ne_binary() | atom(), update_options()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
update_category('undefined', _, _, _, _) -> 'ok';
update_category(_, 'undefined', _, _, _) -> 'ok';
update_category(_, _, 'undefined', _, _) -> 'ok';
update_category(Category, Key, Value, 'undefined', Options) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, Options);
update_category(Category, Key, Value, Node, Options) when not is_list(Key) ->
    update_category(Category, [kz_util:to_binary(Key)], Value, Node, Options);
update_category(Category, Key, Value, Node, Options) when not is_binary(Category) ->
    update_category(kz_util:to_binary(Category), Key, Value, Node, Options);
update_category(Category, Key, Value, Node, Options) when not is_binary(Node) ->
    update_category(Category, Key, Value, kz_util:to_binary(Node), Options);
update_category(Category, Keys, Value, Node, Options) ->
    lager:debug("setting ~s(~p): ~p", [Category, Keys, Value]),
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, Category) of
        {'ok', JObj} ->
            lager:debug("updating category ~s(~s).~s to ~p", [Category
                                                             ,Node
                                                             ,kz_util:join_binary(Keys)
                                                             ,Value
                                                             ]),
            update_category(Category, Keys, Value, Node, Options, JObj);
        {'error', 'not_found'} ->
            lager:debug("config ~s not found, using empty for now", [Category]),
            update_category(Category, Keys, Value, Node, Options, kz_json:new());
        {'error', _Reason}=E ->
            lager:debug("failed to update category ~s: ~p", [Category, kz_datamgr:format_error(_Reason)]),
            E
    end.

-spec update_category(config_category(), config_key(), any(), ne_binary(), update_options(), kz_json:object())
                     -> {'ok', kz_json:object()}.
update_category(Category, Keys, Value, Node, Options, JObj) ->
    PvtFields = props:get_value('pvt_fields', Options),
    L = [Node | Keys],
    case kz_json:get_value(L, JObj) =/= 'undefined'
        orelse props:is_true('node_specific', Options, 'false')
    of
        'true' ->
            update_category(Category, kz_json:set_value(L, Value, JObj), PvtFields);
        'false' ->
            update_category(Category, kz_json:set_value([?KEY_DEFAULT | Keys], Value, JObj), PvtFields)
    end.

-spec update_category(config_category(), kz_json:object(), api_object()) ->
                             {'ok', kz_json:object()}.
update_category(Category, JObj, PvtFields) ->
    case maybe_save_category(Category, JObj, PvtFields) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            lager:debug("conflict saving ~s, merging and saving", [Category]),
            {'ok', Updated} = kz_datamgr:open_doc(?KZ_CONFIG_DB, Category),
            Merged = kz_json:merge_jobjs(Updated, kz_json:public_fields(JObj)),
            lager:debug("updating from ~s to ~s", [kz_doc:revision(JObj), kz_doc:revision(Merged)]),
            update_category(Category, Merged, PvtFields)
    end.

%% @private
-spec maybe_save_category(ne_binary(), kz_json:object(), api_object()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', 'conflict'}.
-spec maybe_save_category(ne_binary(), kz_json:object(), api_object(), boolean()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', 'conflict'}.
-spec maybe_save_category(ne_binary(), kz_json:object(), api_object(), boolean(), boolean()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', 'conflict'}.
maybe_save_category(Category, JObj, PvtFields) ->
    maybe_save_category(Category, JObj, PvtFields, 'false').

maybe_save_category(Category, JObj, PvtFields, Looped) ->
    maybe_save_category(Category, JObj, PvtFields, Looped, is_locked()).

maybe_save_category(_, JObj, _, _, 'true') ->
    lager:warning("failed to update category, system config database is locked!"),
    lager:warning("please update /etc/kazoo/config.ini or use 'sup kapps_config lock_db <boolean>' to enable system config writes."),
    {'ok', JObj};
maybe_save_category(Category, JObj, PvtFields, Looped, _) ->
    lager:debug("updating configuration category ~s(~s)"
               ,[Category, kz_doc:revision(JObj)]
               ),

    JObj1 = update_pvt_fields(Category, JObj, PvtFields),

    case kz_datamgr:save_doc(?KZ_CONFIG_DB, JObj1) of
        {'ok', SavedJObj}=Ok ->
            lager:debug("saved cat ~s to db ~s (~s)", [Category, ?KZ_CONFIG_DB, kz_doc:revision(SavedJObj)]),
            kz_datamgr:add_to_doc_cache(?KZ_CONFIG_DB, Category, SavedJObj),
            Ok;
        {'error', 'not_found'} when not Looped ->
            lager:debug("attempting to create ~s DB", [?KZ_CONFIG_DB]),
            kz_datamgr:db_create(?KZ_CONFIG_DB),
            maybe_save_category(Category, JObj, PvtFields, 'true');
        {'error', 'conflict'}=E -> E;
        {'error', _R} ->
            lager:warning("unable to update ~s system config doc: ~p", [Category, _R]),
            kz_datamgr:add_to_doc_cache(?KZ_CONFIG_DB, Category, JObj1),
            {'ok', JObj1}
    end.

-spec update_pvt_fields(config_category(), kz_json:object(), api_object()) ->
                               kz_json:object().
update_pvt_fields(Category, JObj, 'undefined') ->
    kz_doc:update_pvt_parameters(
      kz_doc:set_id(JObj, Category)
                                ,?KZ_CONFIG_DB
                                ,[{'type', <<"config">>}]
     );
update_pvt_fields(Category, JObj, PvtFields) ->
    Base = update_pvt_fields(Category, JObj, 'undefined'),
    kz_json:merge_jobjs(Base, PvtFields).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Lock configuration document
%%-----------------------------------------------------------------------------
-spec lock_db() -> 'ok'.
-spec lock_db(text() | boolean()) -> 'ok'.
lock_db() ->
    lock_db('true').

lock_db('true') ->
    kz_config:set('kazoo_apps', 'lock_system_config', 'true');
lock_db('false') ->
    kz_config:unset('kazoo_apps', 'lock_system_config');
lock_db(Value) when is_binary(Value) ->
    lock_db(kz_util:to_atom(Value));
lock_db(Value) ->
    lager:warning("wrong parameter ~p. use either 'true' or 'false'", [Value]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc Check if configuration document locked or not
%%-----------------------------------------------------------------------------
-spec is_locked() -> boolean().
is_locked() ->
    case kz_config:get_atom('kazoo_apps', 'lock_system_config') of
        [] -> 'false';
        [Value] -> Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc Flush the configuration cache
%%-----------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_datamgr:flush_cache_docs(?KZ_CONFIG_DB).

-spec flush(ne_binary()) -> 'ok'.
flush(Category) ->
    kz_datamgr:flush_cache_doc(?KZ_CONFIG_DB, Category).

-spec flush(ne_binary(), ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary() | ne_binaries(), atom() | ne_binary()) -> 'ok'.
flush(Category, Key) ->
    flush(Category, Key, ?KEY_DEFAULT).

flush(Category, Key, 'undefined') ->
    flush(Category, Key, ?KEY_DEFAULT);
flush(Category, Key, Node) when not is_list(Key) ->
    flush(Category, [Key], Node);
flush(Category, Keys, Node) when not is_binary(Category) ->
    flush(kz_util:to_binary(Category), Keys, Node);
flush(Category, Keys, Node) when not is_binary(Node) ->
    flush(Category, Keys, kz_util:to_binary(Node));
flush(Category, Keys, Node) ->
    case get_category(Category) of
        {'error', _} -> 'ok';
        {'ok', JObj} ->
            J = kz_json:delete_key([Node | Keys], JObj),
            _ = kz_datamgr:add_to_doc_cache(?KZ_CONFIG_DB, Category, J),
            'ok'
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Fetch a given configuration category from (in order):
%% 1. from the cache
%% 2. from the db
%% 3. from a flat file
%% @end
%%-----------------------------------------------------------------------------
-spec get_category(ne_binary()) -> fetch_ret().
-spec get_category(ne_binary(), boolean()) -> fetch_ret().
get_category(Category) ->
    get_category(Category, 'true').

get_category(Category, 'true') ->
    kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, Category, [{'cache_failures', ['not_found']}]);
get_category(Category, 'false') ->
    kz_datamgr:open_doc(?KZ_CONFIG_DB, Category).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will move a system config setting from one location
%%  to another.  It will create the document if it does not already
%%  exist and will move per-node settings if they exist.
%%  In the event that both the source and destination exist but
%%  have different values it will not make any change.  The parameter
%%  is only removed from the source after a successsful save of the
%%  the destiation.
%% @end
%%--------------------------------------------------------------------
-type migrate_setting() :: {ne_binary(), config_key()}.
-type migrate_value() :: {ne_binary(), ne_binary(), config_key(), _}.
-type migrate_values() :: [migrate_value()].

-define(CONFIG_MIGRATIONS
       ,[{{<<"callflow">>, <<"default_emergency_cid_number">>}
         ,{<<"stepswitch">>, <<"default_emergency_cid_number">>}
         }
        ,{{<<"callflow">>, <<"ensure_valid_emergency_number">>}
         ,{<<"stepswitch">>, <<"ensure_valid_emergency_cid">>}
         }
        ,{{<<"trunkstore">>, <<"ensure_valid_emergency_number">>}
         ,{<<"stepswitch">>, <<"ensure_valid_emergency_cid">>}
         }
        ,{{<<"callflow">>, <<"default_caller_id_number">>}
         ,{<<"kazoo_endpoint">>, <<"default_caller_id_number">>}
         }
        ,{{<<"callflow">>, <<"default_caller_id_name">>}
         ,{<<"kazoo_endpoint">>, <<"default_caller_id_name">>}
         }
        ,{{<<"callflow">>, <<"default_can_text_self">>}
         ,{<<"kazoo_endpoint">>, <<"default_can_text_self">>}
         }
        ,{{<<"callflow">>, <<"restrict_to_known_types">>}
         ,{<<"kazoo_endpoint">>, <<"restrict_to_known_types">>}
         }
        ,{{<<"callflow">>, <<"sip_transport">>}
         ,{<<"kazoo_endpoint">>, <<"sip_transport">>}
         }
        ,{{<<"callflow">>, <<"custom_sip_interface">>}
         ,{<<"kazoo_endpoint">>, <<"custom_sip_interface">>}
         }
        ,{{<<"callflow">>, <<"should_add_diversion_header">>}
         ,{<<"kazoo_endpoint">>, <<"should_add_diversion_header">>}
         }
        ,{{<<"callflow">>, <<"default_ignore_completed_elsewhere">>}
         ,{<<"kazoo_endpoint">>, <<"default_ignore_completed_elsewhere">>}
         }
        ,{{<<"callflow.mobile">>, <<"create_sip_endpoint">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"create_sip_endpoint">>}
         }
        ,{{<<"callflow.mobile">>, <<"codecs">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"codecs">>}
         }
        ,{{<<"callflow.mobile">>, <<"custom_sip_interface">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"custom_sip_interface">>}
         }
        ,{{<<"callflow.mobile">>, <<"formatter">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"formatter">>}
         }
        ,{{<<"callflow.mobile">>, <<"prefix">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"prefix">>}
         }
        ,{{<<"callflow.mobile">>, <<"suffix">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"suffix">>}
         }
        ,{{<<"callflow.mobile">>, <<"realm">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"realm">>}
         }
        ,{{<<"callflow.mobile">>, <<"path">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"path">>}
         }
        ,{{<<"callflow.mobile">>, <<"sms_interface">>}
         ,{<<"kazoo_endpoint.mobile">>, <<"sms_interface">>}
         }
%%         ,{{<<"callflow.mobile">>, [<<"sms">>, <<"connections">>]}
%%          ,{<<"kazoo_endpoint.mobile">>, [<<"sms">>, <<"connections">>]}
%%          }
        ,{{<<"callflow">>, <<"recorder_module">>}
         ,{<<"kazoo_endpoint">>, <<"recorder_module">>}
         }
        ]).

-spec migrate() -> 'ok'.
migrate() ->
    _ = [migrate_config_setting(From, To)
         || {From, To} <- ?CONFIG_MIGRATIONS
        ],
    'ok'.

-spec migrate_config_setting(migrate_setting(), migrate_setting()) ->
                                    'ok' |
                                    {'error', any()}.
migrate_config_setting(From, To) ->
    case remove_config_setting(From) of
        {'ok', _, []} -> 'ok';
        {'ok', JObj, Removed} ->
            migrate_config_setting(JObj, Removed, To);
        {'error', 'not_found'} -> 'ok';
        {'error', Reason} -> {'error', {'remove', Reason}}
    end.

-spec migrate_config_setting(kz_json:object(), migrate_values(), migrate_setting()) ->
                                    'ok' |
                                    {'error', any()}.
migrate_config_setting(UpdatedFrom, Removed, To) ->
    case add_config_setting(To, Removed) of
        {'ok', UpdatedTo} ->
            {'ok', _} = kz_datamgr:save_doc(?KZ_CONFIG_DB, UpdatedTo),
            {'ok', _} = kz_datamgr:save_doc(?KZ_CONFIG_DB, UpdatedFrom),
            'ok';
        {'error', Reason} -> {'error', {'add', Reason}}
    end.

-spec add_config_setting(migrate_setting(), migrate_values()) ->
                                'ok' |
                                {'error', any()}.
add_config_setting({Id, Setting}, Values) ->
    add_config_setting(Id, Setting, Values).

-spec add_config_setting(ne_binary(), config_key(), migrate_values()) ->
                                'ok' |
                                {'error', any()}.
add_config_setting(Id, Setting, Values) when is_binary(Id) ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, Id) of
        {'ok', JObj} -> add_config_setting(JObj, Setting, Values);
        {'error', 'not_found'} ->
            add_config_setting(
              kz_doc:update_pvt_parameters(
                kz_doc:set_id(kz_json:new(), Id)
                                          ,?KZ_CONFIG_DB
                                          ,[{'type', <<"config">>}]
               )
                              ,Setting
                              ,Values
             );
        {'error', _}=Error -> Error
    end;
add_config_setting(JObj, _, []) -> {'ok', JObj};
add_config_setting(JObj, ToSetting, [{FromId, Node, FromSetting, Value} | Values]) ->
    ToId  = kz_doc:id(JObj),
    Key = config_setting_key(Node, ToSetting),
    case kz_json:get_value(Key, JObj) of
        'undefined' ->
            io:format(
              "migrating setting from ~s ~s.~s to ~s ~s.~s value ~p~n"
                     ,[FromId, Node, FromSetting
                      ,ToId, Node, ToSetting
                      ,Value
                      ]
             ),
            add_config_setting(
              kz_json:set_value(Key, Value, JObj)
                              ,ToSetting
                              ,Values
             );
        Value -> add_config_setting(JObj, ToSetting, Values);
        _Else ->
            io:format("the system tried to move the parameter listed below but found a different setting already there, you need to correct this disparity manually!~n", []),
            io:format("  Source~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[?KZ_CONFIG_DB, FromId, Node, FromSetting, Value]
                     ),
            io:format("  Destination~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[?KZ_CONFIG_DB, ToId, Node, ToSetting, _Else]
                     ),
            {'error', 'disparity'}
    end.

-spec remove_config_setting(migrate_setting()) ->
                                   {'ok', kz_json:object(), migrate_values()} |
                                   {'error', any()}.
remove_config_setting({Id, Setting}) ->
    remove_config_setting(Id, Setting).

-spec remove_config_setting(ne_binary() | kz_json:object(), config_key()) ->
                                   {'ok', kz_json:object(), migrate_values()} |
                                   {'error', any()}.
remove_config_setting(Id, Setting) when is_binary(Id) ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, Id) of
        {'ok', JObj} -> remove_config_setting(JObj, Setting);
        {'error', _}=Error -> Error
    end;
remove_config_setting(JObj, Setting) ->
    Id = kz_doc:id(JObj),
    Keys =
        [{Id, Node, Setting}
         || Node <- kz_json:get_public_keys(JObj)
        ],
    remove_config_setting(Keys, JObj, []).

-spec remove_config_setting([{ne_binary(), ne_binary(), config_key()}], kz_json:object(), migrate_values()) ->
                                   {'ok', kz_json:object(), migrate_values()}.
remove_config_setting([], JObj, Removed) ->
    {'ok', JObj, Removed};
remove_config_setting([{Id, Node, Setting} | Keys], JObj, Removed) ->
    Key = config_setting_key(Node, Setting),
    case kz_json:get_value(Key, JObj) of
        'undefined' -> remove_config_setting(Keys, JObj, Removed);
        Value ->
            remove_config_setting(Keys
                                 ,kz_json:delete_key(Key, JObj)
                                 ,[{Id, Node, Setting, Value} | Removed]
                                 )
    end.

-spec config_setting_key(ne_binary(), config_key()) -> ne_binaries().
%% NOTE: to support nested keys, update this merge function
config_setting_key(Node, Setting) ->
    [Node, Setting].
