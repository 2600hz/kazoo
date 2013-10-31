%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%% {Pids, _} = lists:split(10, lists:reverse(lists:sort([{process_info(P, memory), P} || P <-  erlang:processes()]))), [process_info(P, backtrace) || P <- Pids]
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

-export([set/3, set/4, set_default/3, set_node/4]).
-export([flush/0, flush/1, flush/2, flush/3]).

-type config_category() :: ne_binary() | nonempty_string() | atom().
-type config_key() :: ne_binary() | nonempty_string() | atom() | [config_key(),...].

-type fetch_ret() :: {'ok', wh_json:object()} |
                     {'error', 'not_found'}.

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
-spec get_non_empty(config_category(), config_key()) -> api_boolean().
-spec get_non_empty(config_category(), config_key(), Default) -> boolean() | Default.
-spec get_non_empty(config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_non_empty(Category, Key) ->
    get_non_empty(Category, Key, 'undefined').

get_non_empty(Category, Key, Default) ->
    get_non_empty(Category, Key, Default, wh_util:to_binary(node())).
get_non_empty(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case wh_util:is_empty(Value) of
        'true' -> 'undefined';
        'false' -> Value
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
-spec get(config_category(), config_key()) -> term() | 'undefined'.
-spec get(config_category(), config_key(), Default) -> term() | Default.
-spec get(config_category(), config_key(), Default, ne_binary() | atom()) -> term() | Default.

get(Category, Key) ->
    get(Category, Key, 'undefined').

get(Category, Key, Default) ->
    get(Category, Key, Default, node()).

get(Category, Key, Default, 'undefined') ->
    get(Category, Key, Default, <<"default">>);
get(Category, Key, Default, Node) when not is_list(Key) ->
    get(Category, [wh_util:to_binary(Key)], Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Category) ->
    get(wh_util:to_binary(Category), Keys, Default, Node);
get(Category, Keys, Default, Node) when not is_binary(Node) ->
    get(Category, Keys, Default, wh_util:to_binary(Node));
get(Category, Keys, Default, Node) ->
    case get_category(Category) of
        {'ok', JObj} -> get_value(Category, Node, Keys, Default, JObj);
        {'error', _} ->
            lager:debug("missing category ~s(default) ~p: ~p", [Category, Keys, Default]),
            _ = set(Category, Keys, Default),
            Default
    end.

-spec get_value(config_category(), config_key(), config_key(), Default, wh_json:object()) ->
                         Default | term().
get_value(Category, <<"default">>, Keys, Default, JObj) ->
    get_default_value(Category, Keys, Default, JObj);
get_value(Category, Node, Keys, Default, JObj) ->
    case wh_json:get_value([Node | Keys], JObj) of
        'undefined' -> get_default_value(Category, Keys, Default, JObj);
        Else -> Else
    end.

-spec get_default_value(config_category(), config_key(), Default, wh_json:object()) ->
                                 Default | term().
get_default_value(Category, Keys, Default, JObj) ->
    case wh_json:get_value([<<"default">> | Keys], JObj) of
        'undefined' ->
            _ = set(Category, Keys, Default),
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
    case wh_json:get_value(<<"default">>, JObj) of
        'undefined' -> [];
        DefJObj -> wh_json:to_proplist(DefJObj)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but specific to this node
%% @end
%%-----------------------------------------------------------------------------
-spec set(config_category(), config_key(), term()) ->
                 {'ok', wh_json:object()}.
set(Category, Key, Value) ->
    set(Category, Key, Value, node()).

-spec set(config_category(), config_key(), term(), ne_binary() | atom()) ->
                 {'ok', wh_json:object()}.
set(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, []).

-spec set_default(config_category(), config_key(), term()) ->
                         {'ok', wh_json:object()} | 'ok'.
set_default(Category, Key, Value) ->
    update_category(Category, Key, Value, <<"default">>, []).

-spec set_node(config_category(), config_key(), term(), ne_binary() | atom()) ->
                 {'ok', wh_json:object()}.
set_node(Category, _, _, 'undefined') -> get_category(Category);
set_node(Category, Key, Value, Node) ->
    update_category(Category, Key, Value, Node, [{'node_specific', 'true'}]).

-spec update_category(config_category(), config_key(), term(), ne_binary() | atom(), wh_proplist()) ->
                             {'ok', wh_json:object()}.
update_category('undefined', _, _, _, _) -> 'ok';
update_category(_, 'undefined', _, _, _) -> 'ok';
update_category(_, _, 'undefined', _, _) -> 'ok';
update_category(Category, Key, Value, 'undefined', Opts) ->
    update_category(Category, Key, Value, <<"default">>, Opts);
update_category(Category, Key, Value, Node, Opts) when not is_list(Key) ->
    update_category(Category, [wh_util:to_binary(Key)], Value, Node, Opts);
update_category(Category, Key, Value, Node, Opts) when not is_binary(Category) ->
    update_category(wh_util:to_binary(Category), Key, Value, Node, Opts);
update_category(Category, Key, Value, Node, Opts) when not is_binary(Node) ->
    update_category(Category, Key, Value, wh_util:to_binary(Node), Opts);
update_category(Category, Keys, Value, Node, Opts) ->
    lager:debug("setting ~s(~p): ~p", [Category, Keys, Value]),
    case couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {'ok', JObj} -> update_category(Category, Keys, Value, Node, Opts, JObj);
        {'error', 'not_found'} ->
            update_category(Category, Keys, Value, Node, Opts, wh_json:new());
        {'error', _Reason} ->
            lager:warning("failed to find ~s in system config (just updating cache): ~p", [Category, _Reason]),
            case wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, category_key(Category)) of
                {'error', 'not_found'}=E -> E;
                {'ok', JObj} -> update_category(Category, Keys, Value, Node, Opts, JObj)
            end
    end.

-spec update_category(config_category(), config_key(), term(), ne_binary() | atom(), wh_proplist(), wh_json:object()) ->
                             {'ok', wh_json:object()}.
update_category(Category, Keys, Value, Node, Opts, JObj) ->
    case wh_json:get_value([Node | Keys], JObj) =/= 'undefined'
        orelse props:is_true('node_specific', Opts, 'false')
    of
        'true' ->
            J = wh_json:set_value([Node | Keys], Value, JObj),
            maybe_save_category(Category, J);
        'false' ->
            J = wh_json:set_value([<<"default">> | Keys], Value, JObj),
            maybe_save_category(Category, J)
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%-----------------------------------------------------------------------------
maybe_save_category(Category, JObj) ->
    maybe_save_category(Category, JObj, 'false').

maybe_save_category(Category, JObj, Looped) ->
    lager:debug("updating configuration category ~s", [Category]),
    JObj1 = wh_json:set_value(<<"_id">>, Category, JObj),
    case couch_mgr:save_doc(?WH_CONFIG_DB, JObj1) of
        {'ok', SavedJObj} ->
            lager:debug("saved cat ~s to db ~s", [Category, ?WH_CONFIG_DB]),
            cache_jobj(Category, SavedJObj);
        {'error', 'not_found'} when not Looped ->
            lager:debug("attempting to create ~s DB", [?WH_CONFIG_DB]),
            couch_mgr:db_create(?WH_CONFIG_DB),
            maybe_save_category(Category, JObj, 'true');
        {'error', _R} ->
            lager:warning("unable to update ~s system config doc: ~p", [Category, _R]),
            cache_jobj(Category, JObj1)
    end.    

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% flush the configuration cache
%% @end
%%-----------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?WHAPPS_CONFIG_CACHE).

-spec flush(ne_binary()) -> 'ok'.
flush(Category) ->
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, category_key(Category)).

-spec flush(ne_binary(), ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary() | ne_binaries(), atom() | ne_binary()) -> 'ok'.
flush(Category, Key) ->
    flush(Category, Key, <<"default">>).

flush(Category, Key, 'undefined') ->
    flush(Category, Key, <<"default">>);
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
            _ = cache_jobj(Category, J),
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
    Routines = [fun get_config_cache/1
                ,fun get_db_config/1
               ],
    lists:foldl(fun(_, {'ok', _}=Acc) -> Acc;
                   (F, _) -> F(Category)
                end, {'error', 'not_found'}, Routines).

-spec get_config_cache(ne_binary()) -> fetch_ret().
get_config_cache(Category) ->
    wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, category_key(Category)).

-spec get_db_config(ne_binary()) -> fetch_ret().
get_db_config(Category) ->
    lager:debug("fetch db config for ~s", [Category]),
    case couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {'ok', JObj} -> cache_jobj(Category, JObj);
        {'error', _R}=E ->
            lager:debug("could not fetch config ~s from db: ~p", [Category, _R]),
            E
    end.
 
%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec cache_jobj(ne_binary(), wh_json:object()) ->
                        {'ok', wh_json:object()}.
cache_jobj(Category, JObj) ->
    lager:debug("stored ~s into whapps config cache", [Category]),
    CacheProps = [{'expires', 'infinity'}
                  ,{'origin', {'db', ?WH_CONFIG_DB, Category}}
                 ],
    wh_cache:store_local(?WHAPPS_CONFIG_CACHE
                         ,category_key(Category)
                         ,JObj
                         ,CacheProps),
    {'ok', JObj}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% convert the category name to a corresponding flat file that contains those
%% initial (default) settings
%% @end
%%-----------------------------------------------------------------------------
-spec category_to_file(ne_binary()) -> iolist() | 'undefined'.
category_to_file(<<"notify.voicemail_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_vm.config"];
category_to_file(<<"notify.fax_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax.config"];
category_to_file(<<"notify.deregister">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_deregister.config"];
category_to_file(<<"notify.password_recovery">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_pwd_recovery.config"];
category_to_file(<<"notify.new_account">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_new_account.config"];
category_to_file(<<"notify.first_occurrence">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_first_occurrence.config"];
category_to_file(<<"notify.cnam_request">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_cnam_request.config"];
category_to_file(<<"notify.port_request">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_port_request.config"];
category_to_file(<<"notify.ported">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_ported.config"];
category_to_file(<<"notify.low_balance">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_low_balance.config"];
category_to_file(<<"notify.system_alert">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_system_alert.config"];
category_to_file(<<"notify.transaction">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_transaction.config"];
category_to_file(_) ->
    'undefined'.

-spec category_key(Cat) -> {?MODULE, Cat}.
category_key(Category) ->
    {?MODULE, Category}.
