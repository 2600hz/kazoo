%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_config).

-include("whistle_config.hrl").

-export([get/2, get/3, get/4, get_all_kvs/1]).
-export([get_string/2, get_string/3, get_string/4]).
-export([get_binary/2, get_binary/3, get_binary/4]).
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
-export ([lock_db/0, lock_db/1, is_locked/0]).
-export([flush/0, flush/1, flush/2, flush/3]).

-type config_category() :: ne_binary() | nonempty_string() | atom().
-type config_key() :: ne_binary() | nonempty_string() | atom() | [config_key(),...].

-type update_option() :: {'node_specific', boolean()} |
                         {'pvt_fields', api_object()}.
-type update_options() :: [update_option()].

-type fetch_ret() :: {'ok', wh_json:object()} |
                     {'error', 'not_found'}.

-define(KEY_DEFAULT, <<"default">>).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a list
%% @end
%%-----------------------------------------------------------------------------
-spec get_string(config_category(), config_key()) -> api_string().
-spec get_string(config_category(), config_key(), Default) -> nonempty_string() | Default.
-spec get_string(config_category(), config_key(), Default, ne_binary()) -> nonempty_string() | Default.
get_string(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:to_list(Else)
    end.
get_string(Category, Key, Default) ->
    get_string(Category, Key, Default, wh_util:to_binary(node())).
get_string(Category, Key, Default, Node) ->
    wh_util:to_list(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a binary
%% @end
%%-----------------------------------------------------------------------------
-spec get_binary(config_category(), config_key()) -> api_binary().
-spec get_binary(config_category(), config_key(), Default) -> binary() | Default.
-spec get_binary(config_category(), config_key(), Default, ne_binary()) -> binary() | Default.
get_binary(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:to_binary(Else)
    end.
get_binary(Category, Key, Default) ->
    get_binary(Category, Key, Default, wh_util:to_binary(node())).
get_binary(Category, Key, Default, Node) ->
    wh_util:to_binary(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a atom
%% @end
%%-----------------------------------------------------------------------------
-spec get_atom(config_category(), config_key()) -> api_atom().
-spec get_atom(config_category(), config_key(), Default) -> atom() | Default.
-spec get_atom(config_category(), config_key(), Default, ne_binary()) -> atom() | Default.
get_atom(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:to_atom(Else, 'true')
    end.
get_atom(Category, Key, Default) ->
    get_atom(Category, Key, Default, wh_util:to_binary(node())).
get_atom(Category, Key, Default, Node) ->
    wh_util:to_atom(get(Category, Key, Default, Node), 'true').

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a integer
%% @end
%%-----------------------------------------------------------------------------
-spec get_integer(config_category(), config_key()) -> api_integer().
-spec get_integer(config_category(), config_key(), Default) -> integer() | Default.
-spec get_integer(config_category(), config_key(), Default, ne_binary()) -> integer() | Default.
get_integer(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:to_integer(Else)
    end.
get_integer(Category, Key, Default) ->
    get_integer(Category, Key, Default, wh_util:to_binary(node())).
get_integer(Category, Key, Default, Node) ->
    wh_util:to_integer(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a float
%% @end
%%-----------------------------------------------------------------------------
-spec get_float(config_category(), config_key()) -> api_float().
-spec get_float(config_category(), config_key(), Default) -> float() | Default.
-spec get_float(config_category(), config_key(), Default, ne_binary()) -> float() | Default.
get_float(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:to_float(Else)
    end.
get_float(Category, Key, Default) ->
    get_float(Category, Key, Default, wh_util:to_binary(node())).
get_float(Category, Key, Default, Node) ->
    wh_util:to_float(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_false
%% @end
%%-----------------------------------------------------------------------------
-spec get_is_false(config_category(), config_key()) -> api_boolean().
-spec get_is_false(config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_false(config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_is_false(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:is_false(Else)
    end.
get_is_false(Category, Key, Default) ->
    get_is_false(Category, Key, Default, wh_util:to_binary(node())).
get_is_false(Category, Key, Default, Node) ->
    wh_util:is_false(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_true
%% @end
%%-----------------------------------------------------------------------------
-spec get_is_true(config_category(), config_key()) -> api_boolean().
-spec get_is_true(config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_true(config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_is_true(Category, Key) ->
    case get(Category, Key) of
        'undefined' -> 'undefined';
        Else -> wh_util:is_true(Else)
    end.
get_is_true(Category, Key, Default) ->
    get_is_true(Category, Key, Default, wh_util:to_binary(node())).
get_is_true(Category, Key, Default, Node) ->
    wh_util:is_true(get(Category, Key, Default, Node)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_true
%% @end
%%-----------------------------------------------------------------------------
-spec get_non_empty(config_category(), config_key()) -> _ | 'undefined'.
-spec get_non_empty(config_category(), config_key(), Default) -> _ | Default.
-spec get_non_empty(config_category(), config_key(), Default, ne_binary()) -> _ | Default.
get_non_empty(Category, Key) ->
    get_non_empty(Category, Key, 'undefined').

get_non_empty(Category, Key, Default) ->
    get_non_empty(Category, Key, Default, wh_util:to_binary(node())).
get_non_empty(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case wh_util:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

-spec get_ne_binary(config_category(), config_key()) -> api_binary().
-spec get_ne_binary(config_category(), config_key(), Default) -> ne_binary() | Default.
-spec get_ne_binary(config_category(), config_key(), Default, ne_binary()) -> ne_binary() | Default.
get_ne_binary(Category, Key) ->
    get_ne_binary(Category, Key, 'undefined').

get_ne_binary(Category, Key, Default) ->
    get_ne_binary(Category, Key, Default, wh_util:to_binary(node())).
get_ne_binary(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case wh_util:is_empty(Value) of
        'true' -> Default;
        'false' -> wh_util:to_binary(Value)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category
%%
%% Also, when looking up the key see if there is a value specific to this
%% node but if there is not then use the default value.
%% @end
%%-----------------------------------------------------------------------------
-spec get(config_category(), config_key()) -> any() | 'undefined'.
-spec get(config_category(), config_key(), Default) -> any() | Default.
-spec get(config_category(), config_key(), Default, ne_binary() | atom()) -> any() | Default.

get(Category, Key) ->
    get(Category, Key, 'undefined').

get(Category, Key, Default) ->
    get(Category, Key, Default, node()).

get(Category, Key, Default, 'undefined') ->
    get(Category, Key, Default, ?KEY_DEFAULT);
get(Category, Key, Default, Node) when not is_list(Key) ->
    get(Category, [wh_util:to_binary(Key)], Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Category) ->
    get(wh_util:to_binary(Category), Keys, Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Node) ->
    get(Category, Keys, Default, wh_util:to_binary(Node));
get(Category, Keys, Default, Node) ->
    case get_category(Category) of
        {'ok', JObj} -> get_value(Category, Node, Keys, Default, JObj);
        {'error', 'timeout'} ->
            Default;
        {'error', _} ->
            lager:debug("missing category ~s(default) ~p: ~p", [Category, Keys, Default]),
            _ = set(Category, Keys, Default),
            Default
    end.

-spec get_value(config_category(), config_key(), config_key(), Default, wh_json:object()) ->
                         Default | any().
get_value(Category, ?KEY_DEFAULT, Keys, Default, JObj) ->
    get_default_value(Category, Keys, Default, JObj);
get_value(Category, Node, Keys, Default, JObj) ->
    case wh_json:get_value([Node | Keys], JObj) of
        'undefined' -> get_zone_value(Category, Node, Keys, Default, JObj);
        Else -> Else
    end.

-spec get_zone_value(config_category(), config_key(), config_key(), Default, wh_json:object()) ->
                         Default | any().
get_zone_value(Category, _Node, Keys, Default, JObj) ->
    Zone = wh_util:to_binary(wh_nodes:local_zone()),
    case wh_json:get_value([Zone | Keys], JObj) of
        'undefined' -> get_default_value(Category, Keys, Default, JObj);
        Else -> Else
    end.

-spec get_default_value(config_category(), config_key(), Default, wh_json:object()) ->
                                 Default | _.
get_default_value(Category, Keys, Default, JObj) ->
    case wh_json:get_value([?KEY_DEFAULT | Keys], JObj) of
        'undefined' ->
            lager:debug("setting default for ~s ~p: ~p", [Category, Keys, Default]),
            _ = set_default(Category, Keys, Default),
            Default;
        Else -> Else
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get all Key-Value pairs for a given category
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_kvs(ne_binary()) -> wh_proplist().
get_all_kvs(Category) ->
    case get_category(Category) of
        {'error', _} -> [];
        {'ok', JObj} -> get_all_kvs(wh_util:to_binary(node()), JObj)
    end.

-spec get_all_kvs(ne_binary(), wh_json:object()) -> wh_proplist().
get_all_kvs(Node, JObj) ->
    case wh_json:get_value(Node, JObj) of
        'undefined' -> get_all_default_kvs(JObj);
        NodeJObj -> wh_json:to_proplist(NodeJObj)
    end.

-spec get_all_default_kvs(wh_json:object()) -> wh_proplist().
get_all_default_kvs(JObj) ->
    case wh_json:get_value(?KEY_DEFAULT, JObj) of
        'undefined' -> [];
        DefJObj -> wh_json:to_proplist(DefJObj)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but specific to this node
%% @end
%%-----------------------------------------------------------------------------
-spec set(config_category(), config_key(), any()) ->
                 {'ok', wh_json:object()}.
set(Category, Key, Value) ->
    set(Category, Key, Value, node()).

-spec set(config_category(), config_key(), any(), ne_binary() | atom()) ->
                 {'ok', wh_json:object()}.
set(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, []).

-spec set_default(config_category(), config_key(), any()) ->
                         {'ok', wh_json:object()} | 'ok' |
                         {'error', any()}.
set_default(Category, Key, Value) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, []).

-spec update_default(config_category(), config_key(), wh_json:json_term()) ->
                            {'ok', wh_json:object()} | 'ok' |
                            {'error', any()}.
-spec update_default(config_category(), config_key(), wh_json:json_term(), update_options()) ->
                            {'ok', wh_json:object()} | 'ok' |
                            {'error', any()}.
update_default(Category, Key, Value) ->
    update_default(Category, Key, Value, []).
update_default(Category, Key, Value, Options) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, Options).

-spec set_node(config_category(), config_key(), any(), ne_binary() | atom()) ->
                      {'ok', wh_json:object()}.
set_node(Category, _, _, 'undefined') -> get_category(Category);
set_node(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, [{'node_specific', 'true'}]).

-spec update_category(config_category(), config_key(), any(), ne_binary() | atom(), update_options()) ->
                             {'ok', wh_json:object()} |
                             {'error', any()}.
update_category('undefined', _, _, _, _) -> 'ok';
update_category(_, 'undefined', _, _, _) -> 'ok';
update_category(_, _, 'undefined', _, _) -> 'ok';
update_category(Category, Key, Value, 'undefined', Options) ->
    update_category(Category, Key, Value, ?KEY_DEFAULT, Options);
update_category(Category, Key, Value, Node, Options) when not is_list(Key) ->
    update_category(Category, [wh_util:to_binary(Key)], Value, Node, Options);
update_category(Category, Key, Value, Node, Options) when not is_binary(Category) ->
    update_category(wh_util:to_binary(Category), Key, Value, Node, Options);
update_category(Category, Key, Value, Node, Options) when not is_binary(Node) ->
    update_category(Category, Key, Value, wh_util:to_binary(Node), Options);
update_category(Category, Keys, Value, Node, Options) ->
    lager:debug("setting ~s(~p): ~p", [Category, Keys, Value]),
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, Category) of
        {'ok', JObj} ->
            lager:debug("updating category ~s(~s).~s to ~p", [Category
                                                              ,Node
                                                              ,wh_util:join_binary(Keys)
                                                              ,Value
                                                             ]),
            update_category(Category, Keys, Value, Node, Options, JObj);
        {'error', 'not_found'} ->
            lager:debug("config ~s not found, using empty for now", [Category]),
            update_category(Category, Keys, Value, Node, Options, wh_json:new());
        {'error', _Reason}=E ->
            lager:debug("failed to update category ~s: ~p", [Category, couch_util:format_error(_Reason)]),
            E
    end.

-spec update_category(config_category(), config_key(), any(), ne_binary(), update_options(), wh_json:object())
                     -> {'ok', wh_json:object()}.
update_category(Category, Keys, Value, Node, Options, JObj) ->
    PvtFields = props:get_value('pvt_fields', Options),

    lager:info("attempting to update ~s:~s", [Category, Keys]),
    case wh_json:get_value([Node | Keys], JObj) =/= 'undefined'
        orelse props:is_true('node_specific', Options, 'false')
    of
        'true' ->
            update_category(Category, wh_json:set_value([Node | Keys], Value, JObj), PvtFields);
        'false' ->
            update_category(Category, wh_json:set_value([?KEY_DEFAULT | Keys], Value, JObj), PvtFields)
    end.

-spec update_category(config_category(), wh_json:object(), api_object()) ->
                             {'ok', wh_json:object()}.
update_category(Category, JObj, PvtFields) ->
    case maybe_save_category(Category, JObj, PvtFields) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            lager:debug("conflict saving ~s, merging and saving", [Category]),
            {'ok', Updated} = couch_mgr:open_doc(?WH_CONFIG_DB, Category),
            Merged = wh_json:merge_jobjs(Updated, wh_json:public_fields(JObj)),
            lager:debug("updating from ~s to ~s", [wh_doc:revision(JObj), wh_doc:revision(Merged)]),
            update_category(Category, Merged, PvtFields)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec maybe_save_category(ne_binary(), wh_json:object(), api_object()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'conflict'}.
-spec maybe_save_category(ne_binary(), wh_json:object(), api_object(), boolean()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'conflict'}.
-spec maybe_save_category(ne_binary(), wh_json:object(), api_object(), boolean(), boolean()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'conflict'}.
maybe_save_category(Category, JObj, PvtFields) ->
    maybe_save_category(Category, JObj, PvtFields, 'false').

maybe_save_category(Category, JObj, PvtFields, Looped) ->
    maybe_save_category(Category, JObj, PvtFields, Looped, is_locked()).

maybe_save_category(_Cat, JObj, _, _, 'true') ->
    lager:warning("failed to update ~s, system config database is locked!", [_Cat]),
    lager:warning("please update /etc/kazoo/config.ini or use 'sup whapps_config lock_db <boolean>' to enable system config writes."),
    {'ok', JObj};
maybe_save_category(Category, JObj, PvtFields, Looped, _) ->
    lager:debug("updating configuration category ~s(~s)"
                ,[Category, wh_doc:revision(JObj)]
               ),

    JObj1 = update_pvt_fields(Category, JObj, PvtFields),

    case couch_mgr:save_doc(?WH_CONFIG_DB, JObj1) of
        {'ok', SavedJObj} ->
            lager:debug("saved cat ~s to db ~s (~s)", [Category, ?WH_CONFIG_DB, wh_doc:revision(SavedJObj)]),
            couch_mgr:add_to_doc_cache(?WH_CONFIG_DB, Category, SavedJObj),
            {'ok', SavedJObj};
        {'error', 'not_found'} when not Looped ->
            lager:debug("attempting to create ~s DB", [?WH_CONFIG_DB]),
            couch_mgr:db_create(?WH_CONFIG_DB),
            maybe_save_category(Category, JObj, PvtFields, 'true');
        {'error', 'conflict'}=E -> E;
        {'error', _R} ->
            lager:warning("unable to update ~s system config doc: ~p", [Category, _R]),
            couch_mgr:add_to_doc_cache(?WH_CONFIG_DB, Category, JObj1),
            {'ok', JObj1}
    end.

-spec update_pvt_fields(config_category(), wh_json:object(), api_object()) ->
                               wh_json:object().
update_pvt_fields(Category, JObj, 'undefined') ->
    wh_doc:update_pvt_parameters(
      wh_doc:set_id(JObj, Category)
      ,?WH_CONFIG_DB
      ,[{'type', <<"config">>}]
     );
update_pvt_fields(Category, JObj, PvtFields) ->
    Base = update_pvt_fields(Category, JObj, 'undefined'),
    wh_json:merge_jobjs(Base, PvtFields).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% lock configuration document
%% @end
%%-----------------------------------------------------------------------------
-spec lock_db() -> 'ok'.
-spec lock_db(ne_binary()) -> 'ok'.
lock_db() ->
    lock_db('true').

lock_db('true') ->
    wh_config:set('whistle_apps', 'lock_system_config', 'true');
lock_db('false') ->
    wh_config:unset('whistle_apps', 'lock_system_config');
lock_db(Value) when is_binary(Value) ->
    lock_db(wh_util:to_atom(Value));
lock_db(Value) ->
    lager:warning("wrong parameter ~p. use either 'true' or 'false'", [Value]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% check if configuration document locked or not
%% @end
%%-----------------------------------------------------------------------------
-spec is_locked() -> boolean().
is_locked() ->
    case wh_config:get_atom('whistle_apps', 'lock_system_config') of
        [] -> 'false';
        [Value] -> Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% flush the configuration cache
%% @end
%%-----------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    couch_mgr:flush_cache_docs(?WH_CONFIG_DB).

-spec flush(ne_binary()) -> 'ok'.
flush(Category) ->
    couch_mgr:flush_cache_doc(?WH_CONFIG_DB, Category).

-spec flush(ne_binary(), ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary() | ne_binaries(), atom() | ne_binary()) -> 'ok'.
flush(Category, Key) ->
    flush(Category, Key, ?KEY_DEFAULT).

flush(Category, Key, 'undefined') ->
    flush(Category, Key, ?KEY_DEFAULT);
flush(Category, Key, Node) when not is_list(Key) ->
    flush(Category, [Key], Node);
flush(Category, Keys, Node) when not is_binary(Category) ->
    flush(wh_util:to_binary(Category), Keys, Node);
flush(Category, Keys, Node) when not is_binary(Node) ->
    flush(Category, Keys, wh_util:to_binary(Node));
flush(Category, Keys, Node) ->
    case get_category(Category) of
        {'error', _} -> 'ok';
        {'ok', JObj} ->
            J = wh_json:delete_key([Node | Keys], JObj),
            _ = couch_mgr:add_to_doc_cache(?WH_CONFIG_DB, Category, J),
            'ok'
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% fetch a given configuration category from (in order):
%% 1. from the cache
%% 2. from the db
%% 3. from a flat file
%% @end
%%-----------------------------------------------------------------------------
-spec get_category(ne_binary()) -> fetch_ret().
get_category(Category) ->
    couch_mgr:open_cache_doc(?WH_CONFIG_DB, Category, [{'cache_failures', ['not_found']}]).
