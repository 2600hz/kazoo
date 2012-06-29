%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 8 Nov 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_config).

-include_lib("whistle_apps/src/whistle_apps.hrl").

-export([get/2, get/3, get/4, get_all_kvs/1]).
-export([get_string/2, get_string/3, get_string/4]).
-export([get_binary/2, get_binary/3, get_binary/4]).
-export([get_atom/2, get_atom/3, get_atom/4]).
-export([get_integer/2, get_integer/3, get_integer/4]).
-export([get_float/2, get_float/3, get_float/4]).
-export([get_is_false/2, get_is_false/3, get_is_false/4]).
-export([get_is_true/2, get_is_true/3, get_is_true/4]).
-export([get_non_empty/2, get_non_empty/3, get_non_empty/4]).

-export([set/3, set/4, set_default/3]).
-export([flush/0, flush/1, flush/2, flush/3
         ,import/1, couch_ready/0
        ]).

-type config_category() :: ne_binary() | nonempty_string() | atom().
-type config_key() :: ne_binary() | nonempty_string() | atom() | [config_key(),...].

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a list
%% @end
%%-----------------------------------------------------------------------------
-spec get_string/2 :: (config_category(), config_key()) -> nonempty_string() | 'undefined'.
-spec get_string/3 :: (config_category(), config_key(), Default) -> nonempty_string() | Default.
-spec get_string/4 :: (config_category(), config_key(), Default, ne_binary()) -> nonempty_string() | Default.
get_string(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_binary/2 :: (config_category(), config_key()) -> binary() | 'undefined'.
-spec get_binary/3 :: (config_category(), config_key(), Default) -> binary() | Default.
-spec get_binary/4 :: (config_category(), config_key(), Default, ne_binary()) -> binary() | Default.
get_binary(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_atom/2 :: (config_category(), config_key()) -> atom() | 'undefined'.
-spec get_atom/3 :: (config_category(), config_key(), Default) -> atom() | Default.
-spec get_atom/4 :: (config_category(), config_key(), Default, ne_binary()) -> atom() | Default.
get_atom(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_atom(Else, true)
    end.
get_atom(Category, Key, Default) ->
    get_atom(Category, Key, Default, wh_util:to_binary(node())).
get_atom(Category, Key, Default, Node) ->
    wh_util:to_atom(get(Category, Key, Default, Node), true).


%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a integer
%% @end
%%-----------------------------------------------------------------------------
-spec get_integer/2 :: (config_category(), config_key()) -> integer() | 'undefined'.
-spec get_integer/3 :: (config_category(), config_key(), Default) -> integer() | Default.
-spec get_integer/4 :: (config_category(), config_key(), Default, ne_binary()) -> integer() | Default.
get_integer(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_float/2 :: (config_category(), config_key()) -> float() | 'undefined'.
-spec get_float/3 :: (config_category(), config_key(), Default) -> float() | Default.
-spec get_float/4 :: (config_category(), config_key(), Default, ne_binary()) -> float() | Default.
get_float(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_is_false/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_is_false/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_false/4 :: (config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_is_false(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_is_true/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_is_true/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
-spec get_is_true/4 :: (config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_is_true(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
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
-spec get_non_empty/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_non_empty/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
-spec get_non_empty/4 :: (config_category(), config_key(), Default, ne_binary()) -> boolean() | Default.
get_non_empty(Category, Key) ->
    get_non_empty(Category, Key, undefined).

get_non_empty(Category, Key, Default) ->
    get_non_empty(Category, Key, Default, wh_util:to_binary(node())).
get_non_empty(Category, Key, Default, Node) ->
    Value = get(Category, Key, Default, Node),
    case wh_util:is_empty(Value) of
        true -> undefined;
        false -> Value
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
-spec get/2 :: (config_category(), config_key()) -> term() | 'undefined'.
-spec get/3 :: (config_category(), config_key(), Default) -> term() | Default.
-spec get/4 :: (config_category(), config_key(), Default, ne_binary() | atom()) -> term() | Default.
get(Category, Key) ->
    get(Category, Key, undefined).
get(Category, Key, Default) ->
    get(Category, Key, Default, node()).

get(Category, Key, Default, Node) when not is_list(Key) ->
    get(Category, [wh_util:to_binary(Key)], Default, Node);
get(Category0, Keys, Default, Node0) ->
    Category = wh_util:to_binary(Category0),
    Node = wh_util:to_binary(Node0),
    case fetch_category(Category, ?WHAPPS_CONFIG_CACHE) of
        {ok, JObj} ->
            case wh_json:get_value([Node | Keys], JObj) of
                undefined ->
                    case wh_json:get_value([<<"default">> | Keys], JObj) of
                        undefined ->
                            lager:debug("missing key ~s(~s) ~p: ~p", [Category, Node, Keys, Default]),
                            _ = set_default(Category, Keys, Default),
                            Default;
                        Else -> Else
                    end;
                Else ->
                    lager:debug("fetched config ~s(~s) ~p: ~p", [Category, Node, Keys, Else]),
                    Else
            end;
        {error, _} ->
            lager:debug("missing category ~s(default) ~p: ~p", [Category, Keys, Default]),
            Default
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get all Key-Value pairs for a given category
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_kvs/1 :: (ne_binary()) -> proplist().
get_all_kvs(Category) ->
    case fetch_category(Category, ?WHAPPS_CONFIG_CACHE) of
        {error, _} ->
            lager:debug("missing category ~s(~s)", [Category, "default"]),
            [];
        {ok, JObj} ->
            Node = wh_util:to_binary(node()),
            case wh_json:get_value(Node, JObj) of
                undefined ->
                    case wh_json:get_value(<<"default">>, JObj) of
                        undefined ->
                            lager:debug("missing category ~s(~s)", [Category, Node]),
                            [];
                        DefJObj ->
                            lager:debug("fetched configs ~s(~s)", [Category, "default"]),
                            wh_json:to_proplist(DefJObj)
                    end;
                NodeJObj ->
                    lager:debug("fetched configs ~s(~s)", [Category, Node]),
                    wh_json:to_proplist(NodeJObj)
            end
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but specific to this node
%% @end
%%-----------------------------------------------------------------------------
-spec set/3 :: (config_category(), config_key(), term()) -> {'ok', wh_json:json_object()}.
-spec set/4 :: (config_category(), config_key(), term(), ne_binary() | atom()) -> {'ok', wh_json:json_object()}.
set(Category, Key, Value) ->
    set(Category, Key, Value, node()).
set(Category, Key, Value, Node) ->
    do_set(Category, Key, Value, Node).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but in the default (global)
%% section
%% @end
%%-----------------------------------------------------------------------------
-spec set_default/3 :: (config_category(), config_key(), term()) -> {'ok', wh_json:json_object()} | 'ok'.
set_default(_Category, _Key, undefined) ->
    ok;
set_default(Category, Key, Value) ->
    do_set(Category, Key, Value, <<"default">>).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% flush the configuration cache
%% @end
%%-----------------------------------------------------------------------------
-spec flush/0 :: () -> 'ok'.
flush() ->
    wh_cache:flush_local(?WHAPPS_CONFIG_CACHE).

-spec flush/1 :: (ne_binary()) -> 'ok'.
flush(Category) ->
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, category_key(Category)).

-spec flush/2 :: (ne_binary(), ne_binary()) -> 'ok'.
-spec flush/3 :: (ne_binary(), ne_binary() | [ne_binary(),...], atom() | ne_binary()) -> 'ok'.
flush(Category, Key) ->
    flush(Category, Key, <<"default">>).

flush(Category, Key, undefined) ->
    flush(Category, Key, <<"default">>);
flush(Category, Key, Node) when not is_list(Key) ->
    flush(Category, [Key], Node);
flush(Category0, Keys, Node0) ->
    Category = wh_util:to_binary(Category0),
    Node = wh_util:to_binary(Node0),

    UpdateFun = fun(J) ->
                        NodeConfig = wh_json:get_value(Node, J, wh_json:new()),
                        wh_json:delete_key(Keys, NodeConfig)
                end,

    {ok, JObj} = wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, category_key(Category)),
    JObj1 = wh_json:set_value(Node, UpdateFun(JObj), JObj),
    {ok, _} = cache_jobj(?WHAPPS_CONFIG_CACHE, Category, JObj1),
    ok.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% import any new configuration options for the given category from the flat
%% file (can be used when a new option is added)
%% @end
%%-----------------------------------------------------------------------------
-spec import/1 :: (config_category()) -> {'ok', wh_json:json_object()}.
import(Category) when not is_binary(Category) ->
    import(wh_util:to_binary(Category));
import(Category) ->
    fetch_file_config(Category, ?WHAPPS_CONFIG_CACHE).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% fetch a given configuration category from (in order):
%% 1. from the cache
%% 2. from the db
%% 3. from a flat file
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_category/2 :: (ne_binary(), pid() | atom()) -> {'ok', wh_json:json_object()} |
                                                           {'error', 'not_found'}.
fetch_category(Category, Cache) ->
    Lookups = [fun fetch_file_config/2
               ,fun fetch_db_config/2
               ,fun(Cat, C) -> wh_cache:peek_local(C, {?MODULE, Cat}) end
              ],
    lists:foldr(fun(_, {ok, _}=Acc) -> Acc;
                   (F, _) -> F(Category, Cache)
                end, {error, not_found}, Lookups).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% try to fetch the configuration category from the database, if successful
%% cache it
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_db_config/2 :: (ne_binary(), pid()) -> {'ok', wh_json:json_object()} | {'error', 'not_found'}.
fetch_db_config(Category, Cache) ->
    lager:debug("fetch db config for ~s", [Category]),
    case couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {ok, JObj}=Ok ->
            wh_cache:store_local(Cache, category_key(Category), JObj),
            Ok;
        {error, _E} ->
            lager:debug("could not fetch config ~s from db: ~p", [Category, _E]),
            {error, not_found}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% try to fetch the configuration category from a flat file, if successful
%% save it to the db and cache it
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_file_config/2 :: (ne_binary(), pid() | atom()) -> {'ok', wh_json:json_object()}.
fetch_file_config(Category, Cache) ->
    File = category_to_file(Category),
    case file:consult(File) of
        {ok, Terms} ->
            JObj = config_terms_to_json(Terms),
            UpdateFun = fun(J) ->
                                lager:debug("initializing ~s from ~s", [Category, File]),
                                DefaultConfig = wh_json:get_value(<<"default">>, J, wh_json:new()),
                                wh_json:merge_jobjs(DefaultConfig, JObj)
                        end,
            update_category_node(Category, <<"default">>, UpdateFun, Cache);
        {error, _}=E ->
            lager:debug("initializing category ~s without configuration: ~p", [Category, E]),
            UpdateFun = fun(J) ->
                                wh_json:set_value(<<"default">>, wh_json:new(), J)
                        end,
            update_category_node(Category, <<"default">>, UpdateFun, Cache)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% convert the result of the file consult into a json object
%% @end
%%-----------------------------------------------------------------------------
-spec config_terms_to_json/1 :: (proplist()) -> wh_json:json_object().
config_terms_to_json(Terms) ->
    wh_json:from_list([{wh_util:to_binary(K), V} || {K, V} <- Terms]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% does the actual work of setting the key to the value in the given category
%% for the given node
%% @end
%%-----------------------------------------------------------------------------
-spec do_set/4 :: (config_category(), config_key(), term(), ne_binary() | atom()) -> {'ok', wh_json:json_object()}.
do_set(Category, Key, Value, Node) when not is_list(Key) ->
    do_set(Category, [wh_util:to_binary(Key)], Value, Node);
do_set(Category0, Keys, Value, Node0) ->
    Category = wh_util:to_binary(Category0),
    Node = wh_util:to_binary(Node0),

    lager:debug("setting ~s(~p): ~p", [Category, Keys, Value]),

    UpdateFun = fun(J) ->
                        NodeConfig = wh_json:get_value(Node, J, wh_json:new()),
                        wh_json:set_value(Keys, Value, NodeConfig)
                end,

    update_category_node(Category, Node, UpdateFun, ?WHAPPS_CONFIG_CACHE).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% update the configuration category for a given node in both the db and cache
%% @end
%%-----------------------------------------------------------------------------
-spec update_category_node/4 :: (ne_binary(), ne_binary(), fun((wh_json:json_object()) -> wh_json:json_object()) , pid() | atom()) -> {'ok', wh_json:json_object()}.
update_category_node(Category, Node, UpdateFun, Cache) ->
    DBReady = case wh_cache:fetch_local(Cache, couch_ready_key()) of
                  {ok, true} -> true;
                  _ -> false
              end,
    case DBReady andalso couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {ok, JObj} ->
            case wh_json:set_value(Node, UpdateFun(JObj), JObj) of
                JObj -> {ok, JObj};
                UpdatedCat -> update_category(Category, UpdatedCat, Cache)
            end;
        {error, _E} ->
            lager:debug("failed to find category in DB: ~p", [_E]),
            NewCat = wh_json:set_value(Node, UpdateFun(wh_json:new()), wh_json:new()),
            update_category(Category, NewCat, Cache);
        false ->
            lager:debug("couch_mgr hasn't started; just cache the json object"),
            case wh_cache:peek_local(Cache, category_key(Category)) of
                {ok, JObj} ->
                    case wh_json:set_value(Node, UpdateFun(JObj), JObj) of
                        JObj -> {ok, JObj};
                        UpdatedCat -> cache_jobj(Cache, Category, UpdatedCat)
                    end;
                {error, not_found} ->
                    NewCat = wh_json:set_value(Node, UpdateFun(wh_json:new()), wh_json:new()),
                    cache_jobj(Cache, Category, NewCat)
            end
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% update the entire category in both the db and cache
%% @end
%%-----------------------------------------------------------------------------
-spec update_category/3 :: (ne_binary(), wh_json:json_object(), atom()) -> {'ok', wh_json:json_object()}.
update_category(Category, JObj, Cache) ->
    lager:debug("updating configuration category ~s", [Category]),
    JObj1 = wh_json:set_value(<<"_id">>, Category, JObj),
    couch_mgr:db_create(?WH_CONFIG_DB),
    {ok, SavedJObj} = couch_mgr:ensure_saved(?WH_CONFIG_DB, JObj1),
    lager:debug("saved cat ~s to db ~s", [Category, ?WH_CONFIG_DB]),
    cache_jobj(Cache, Category, SavedJObj).

-spec cache_jobj/3 :: (atom() | pid(), ne_binary(), wh_json:json_object()) -> {'ok', wh_json:json_object()}.
cache_jobj(Cache, Category, JObj) ->
    wh_cache:store_local(Cache, category_key(Category), JObj),
    {ok, JObj}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% convert the category name to a corresponding flat file that contains those
%% initial (default) settings
%% @end
%%-----------------------------------------------------------------------------
-spec category_to_file/1 :: (ne_binary()) -> iolist() | 'undefined'.
category_to_file(<<"whapps_controller">>) ->
    [code:lib_dir(whistle_apps, priv), "/startup.config"];
category_to_file(<<"notify.voicemail_to_email">>) ->
    [code:lib_dir(notify, priv), "/notify_vm.config"];
category_to_file(<<"notify.fax_to_email">>) ->
    [code:lib_dir(notify, priv), "/notify_fax.config"];
category_to_file(<<"notify.deregister">>) ->
    [code:lib_dir(notify, priv), "/notify_deregister.config"];
category_to_file(<<"notify.password_recovery">>) ->
    [code:lib_dir(notify, priv), "/notify_pwd_recovery.config"];
category_to_file(<<"notify.new_account">>) ->
    [code:lib_dir(notify, priv), "/notify_new_account.config"];
category_to_file(<<"notify.first_occurrence">>) ->
    [code:lib_dir(notify, priv), "/notify_first_occurrence.config"];
category_to_file(<<"notify.cnam_request">>) ->
    [code:lib_dir(notify, priv), "/notify_cnam_request.config"];
category_to_file(<<"notify.port_request">>) ->
    [code:lib_dir(notify, priv), "/notify_port_request.config"];
category_to_file(<<"notify.low_balance">>) ->
    [code:lib_dir(notify, priv), "/notify_low_balance.config"];
category_to_file(<<"notify.system_alert">>) ->
    [code:lib_dir(notify, priv), "/notify_system_alert.config"];
category_to_file(<<"notify.transaction">>) ->
    [code:lib_dir(notify, priv), "/notify_transaction.config"];
category_to_file(<<"smtp_client">>) ->
    [code:lib_dir(whistle_apps, priv), "/smtp_client.config"];
category_to_file(<<"alerts">>) ->
    [code:lib_dir(whistle_apps, priv), "/alerts.config"];
category_to_file(<<"crossbar">>) ->
    [code:lib_dir(crossbar, priv), "/crossbar.config"];
category_to_file(<<"crossbar.devices">>) ->
    [code:lib_dir(crossbar, priv), "/devices/devices.config"];
category_to_file(<<"crossbar.shared_auth">>) ->
    [code:lib_dir(crossbar, priv), "/shared_auth/shared_auth.config"];
category_to_file(_) ->
    undefined.

couch_ready() ->
    case whereis(couch_mgr) =:= self() of
        true ->
            wh_cache:store_local(?WHAPPS_CONFIG_CACHE, couch_ready_key(), true
                                 ,fun(_, _, _) ->
                                          couch_mgr:load_configs()
                                  end);
        false ->
            ok
    end.

couch_ready_key() ->
    {?MODULE, couch_mgr_ready}.

-spec category_key/1 :: (Cat) -> {?MODULE, Cat}.
category_key(Category) ->
    {?MODULE, Category}.
