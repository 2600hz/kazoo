%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 8 Nov 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_config).

-include("whistle_apps.hrl").

-export([get/2, get/3, get_all_kvs/1]).
-export([get_string/2, get_string/3]).
-export([get_binary/2, get_binary/3]).
-export([get_atom/2, get_atom/3]).
-export([get_integer/2, get_integer/3]).
-export([get_float/2, get_float/3]).
-export([get_is_false/2, get_is_false/3]).
-export([get_is_true/2, get_is_true/3]).
-export([get_non_empty/2, get_non_empty/3]).
-export([set/3, set_default/3]).
-export([flush/0, import/1]).

-type config_category() :: binary() | string() | atom().
-type config_key() :: binary() | string() | atom().

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a list
%% @end
%%-----------------------------------------------------------------------------
-spec get_string/2 :: (config_category(), config_key()) -> nonempty_string() | 'undefined'.
-spec get_string/3 :: (config_category(), config_key(), Default) -> nonempty_string() | Default.
get_string(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_list(Else)
    end.
get_string(Category, Key, Default) ->
    wh_util:to_list(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a binary
%% @end
%%-----------------------------------------------------------------------------
-spec get_binary/2 :: (config_category(), config_key()) -> binary() | 'undefined'.
-spec get_binary/3 :: (config_category(), config_key(), Default) -> binary() | Default.
get_binary(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_binary(Else)
    end.
get_binary(Category, Key, Default) ->
    wh_util:to_binary(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a atom
%% @end
%%-----------------------------------------------------------------------------
-spec get_atom/2 :: (config_category(), config_key()) -> atom() | 'undefined'.
-spec get_atom/3 :: (config_category(), config_key(), Default) -> atom() | Default.
get_atom(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_atom(Else, true)
    end.
get_atom(Category, Key, Default) ->
    wh_util:to_atom(get(Category, Key, Default), true).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a integer
%% @end
%%-----------------------------------------------------------------------------
-spec get_integer/2 :: (config_category(), config_key()) -> integer() | 'undefined'.
-spec get_integer/3 :: (config_category(), config_key(), Default) -> integer() | Default.
get_integer(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_integer(Else)
    end.
get_integer(Category, Key, Default) ->
    wh_util:to_integer(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a float
%% @end
%%-----------------------------------------------------------------------------
-spec get_float/2 :: (config_category(), config_key()) -> float() | 'undefined'.
-spec get_float/3 :: (config_category(), config_key(), Default) -> float() | Default.
get_float(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:to_float(Else)
    end.
get_float(Category, Key, Default) ->
    wh_util:to_float(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_false
%% @end
%%-----------------------------------------------------------------------------
-spec get_is_false/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_is_false/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
get_is_false(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:is_false(Else)
    end.
get_is_false(Category, Key, Default) ->
    wh_util:is_false(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_true
%% @end
%%-----------------------------------------------------------------------------
-spec get_is_true/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_is_true/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
get_is_true(Category, Key) ->
    case get(Category, Key) of
        undefined -> undefined;
        Else -> wh_util:is_true(Else)
    end.
get_is_true(Category, Key, Default) ->
    wh_util:is_true(get(Category, Key, Default)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% get a configuration key for a given category and cast it as a is_true
%% @end
%%-----------------------------------------------------------------------------
-spec get_non_empty/2 :: (config_category(), config_key()) -> boolean() | 'undefined'.
-spec get_non_empty/3 :: (config_category(), config_key(), Default) -> boolean() | Default.
get_non_empty(Category, Key) ->
    get_non_empty(Category, Key, undefined).

get_non_empty(Category, Key, Default) ->
    Value = get(Category, Key, Default),
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
get(Category, Key) ->
    get(Category, Key, undefined).

get(Category, Key, Default) when not is_binary(Category)->
    get(wh_util:to_binary(Category), Key, Default);
get(Category, Key, Default) when not is_binary(Key)->
    get(Category, wh_util:to_binary(Key), Default);
get(Category, Key, Default) ->
    {ok, Cache} = whistle_apps_sup:config_cache_proc(),
    case fetch_category(Category, Cache) of
        {ok, JObj} ->
            Node = wh_util:to_binary(node()),
            case wh_json:get_value([Node, Key], JObj) of
                undefined ->
                    case wh_json:get_value([<<"default">>, Key], JObj) of
                        undefined ->
                            ?LOG("missing key ~s(~s) ~s: ~p", [Category, Node, Key, Default]),
                            Default =/= undefined andalso
                                ?MODULE:set(Category, Key, Default),
                            Default;
                        Else ->
                            ?LOG("fetched config ~s(~s) ~s: ~p", [Category, "default", Key, Else]),
                            Else
                    end;
                Else ->
                    ?LOG("fetched config ~s(~s) ~s: ~p", [Category, Node, Key, Else]),
                    Else
            end;
        {error, _} ->
            ?LOG("missing category ~s(~s) ~s: ~p", [Category, "default", Key, Default]),
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
    {ok, Cache} = whistle_apps_sup:config_cache_proc(),
    case fetch_category(Category, Cache) of
        {error, _} ->
            ?LOG("missing category ~s(~s)", [Category, "default"]),
            [];
        {ok, JObj} ->
            Node = wh_util:to_binary(node()),
            case wh_json:get_value(Node, JObj) of
                undefined ->
                    case wh_json:get_value(<<"default">>, JObj) of
                        undefined ->
                            ?LOG("missing category ~s(~s)", [Category, Node]),
                            [];
                        DefJObj ->
                            ?LOG("fetched configs ~s(~s)", [Category, "default"]),
                            wh_json:to_proplist(DefJObj)
                    end;
                NodeJObj ->
                    ?LOG("fetched configs ~s(~s)", [Category, Node]),
                    wh_json:to_proplist(NodeJObj)
            end
    end.
    

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but specific to this node
%% @end
%%-----------------------------------------------------------------------------
-spec set/3 :: (config_category(), config_key(), term()) -> {'ok', json_object()}.
set(Category, Key, Value) ->
    do_set(Category, wh_util:to_binary(node()), Key, Value).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% set the key to the value in the given category but in the default (global)
%% section
%% @end
%%-----------------------------------------------------------------------------
-spec set_default/3 :: (config_category(), config_key(), term()) -> {'ok', json_object()}.
set_default(Category, Key, Value) ->
    do_set(Category, <<"default">>, Key, Value).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% flush the configuration cache
%% @end
%%-----------------------------------------------------------------------------
-spec flush/0 :: () -> 'ok'.
flush() ->
    {ok, Cache} = whistle_apps_sup:config_cache_proc(),
    wh_cache:flush_local(Cache).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% import any new configuration options for the given category from the flat
%% file (can be used when a new option is added)
%% @end
%%-----------------------------------------------------------------------------
-spec import/1 :: (config_category()) -> {'ok', json_object()}.
import(Category) when not is_binary(Category) ->
    import(wh_util:to_binary(Category));
import(Category) ->
    {ok, Cache} = whistle_apps_sup:config_cache_proc(),
    fetch_file_config(Category, Cache).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% fetch a given configuration category from (in order):
%% 1. from the cache
%% 2. from the db
%% 3. from a flat file
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_category/2 :: (ne_binary(), pid()) -> {'ok', json_object()} | {'error', 'not_found'}.
fetch_category(Category, Cache) ->
    Lookups = [fun fetch_file_config/2
               ,fun fetch_db_config/2
               ,fun(Cat, C) -> wh_cache:peek_local(C, {?MODULE, Cat}) end],
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
-spec fetch_db_config/2 :: (ne_binary(), pid()) -> {'ok', json_object()} | {'error', 'not_found'}.
fetch_db_config(Category, Cache) ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {ok, JObj}=Ok ->
            wh_cache:store_local(Cache, {?MODULE, Category}, JObj),
            Ok;
        {error, _E} ->
            ?LOG("could not fetch config ~s from db: ~p", [Category, _E]),
            {error, not_found}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% try to fetch the configuration category from a flat file, if successful
%% save it to the db and cache it
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_file_config/2 :: (ne_binary(), pid()) -> {'ok', json_object()}.
fetch_file_config(Category, Cache) ->
    File = category_to_file(Category),
    case file:consult(File) of
        {ok, Terms} ->
            JObj = config_terms_to_json(Terms),
            UpdateFun = fun(J) ->
                                ?LOG("initializing ~s from ~s", [Category, File]),
                                DefaultConfig = wh_json:get_value(<<"default">>, J, wh_json:new()),
                                wh_json:merge_jobjs(DefaultConfig, JObj)
                        end,
            update_category_node(Category, <<"default">>, UpdateFun, Cache);
        {error, _}=E ->
            ?LOG("initializing category ~s without configuration: ~p", [Category, E]),
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
-spec config_terms_to_json/1 :: (proplist()) -> json_object().
config_terms_to_json(Terms) ->
    wh_json:from_list([{wh_util:to_binary(K), V} || {K, V} <- Terms]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% does the actual work of setting the key to the value in the given category
%% for the given node
%% @end
%%-----------------------------------------------------------------------------
-spec do_set/4 :: (config_category(), config_key(), config_key(), term()) -> {'ok', json_object()}.
do_set(Category, Node, Key, Value) when not is_binary(Category) ->
    do_set(wh_util:to_binary(Category), Node, Key, Value);
do_set(Category, Node, Key, Value) when not is_binary(Node) ->
    do_set(Category, wh_util:to_binary(Node), Key, Value);
do_set(Category, Node, Key, Value) when not is_binary(Key) ->
    do_set(Category, Node, wh_util:to_binary(Key), Value);
do_set(Category, Node, Key, Value) ->
    {ok, Cache} = whistle_apps_sup:config_cache_proc(),

    UpdateFun = fun(J) ->
                        NodeConfig = wh_json:get_value(Node, J, wh_json:new()),
                        wh_json:set_value(Key, Value, NodeConfig)
                end,

    update_category_node(Category, Node, UpdateFun, Cache).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% update the configuration category for a given node in both the db and cache
%% @end
%%-----------------------------------------------------------------------------
-spec update_category_node/4 :: (ne_binary(), ne_binary(), fun((json_object()) -> json_object()) , pid()) -> {'ok', json_object()}.
update_category_node(Category, Node, UpdateFun , Cache) ->
    case is_pid(whereis(couch_mgr)) andalso couch_mgr:open_doc(?WH_CONFIG_DB, Category) of
        {ok, JObj} ->
            case wh_json:set_value(Node, UpdateFun(JObj), JObj) of
                JObj -> {ok, JObj};
                UpdatedCat -> update_category(Category, UpdatedCat, Cache)
            end;
        {error, _E} ->
            ?LOG("failed to find category in DB: ~p", [_E]),
            NewCat = wh_json:set_value(Node, UpdateFun(wh_json:new()), wh_json:new()),
            update_category(Category, NewCat, Cache);
        false ->
            ?LOG("couch_mgr hasn't started; just cache the json object"),
            case wh_cache:peek_local(Cache, {?MODULE, Category}) of
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
-spec update_category/3 :: (ne_binary(), json_object(), pid()) -> {'ok', json_object()}.
update_category(Category, JObj, Cache) ->
    ?LOG("updating configuration category ~s", [Category]),
    JObj1 = wh_json:set_value(<<"_id">>, Category, JObj),
    {ok, SavedJObj} = couch_mgr:ensure_saved(?WH_CONFIG_DB, JObj1),
    ?LOG("saved cat ~s to db ~s", [Category, ?WH_CONFIG_DB]),
    cache_jobj(Cache, Category, SavedJObj).

cache_jobj(Cache, Category, JObj) ->
    wh_cache:store_local(Cache, {?MODULE, Category}, JObj),
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
category_to_file(<<"notify_vm">>) ->
    [code:lib_dir(notify, priv), "/notify_vm.config"];
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
