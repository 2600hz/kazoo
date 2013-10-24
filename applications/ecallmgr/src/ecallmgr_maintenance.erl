%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_maintenance).

-export([add_fs_node/1, add_fs_node/2
         ,remove_fs_node/1, remove_fs_node/2
         ,list_fs_nodes/0
        ]).
-export([node_summary/0]).
-export([node_details/0
         ,node_details/1
        ]).
-export([authz_summary/0]).
-export([channel_summary/0
         ,channel_summary/1
        ]).
-export([channel_details/0
         ,channel_details/1
        ]).
-export([sync_channels/0
         ,sync_channels/1
        ]).
-export([conference_summary/0
         ,conference_summary/1
        ]).
-export([conference_details/0
         ,conference_details/1
        ]).
-export([sync_conferences/0
         ,sync_conferences/1
        ]).
-export([flush_node_channels/1]).
-export([flush_node_conferences/1]).
-export([flush_registrar/0
         ,flush_registrar/1
         ,flush_registrar/2
        ]).
-export([registrar_summary/0
         ,registrar_summary/1
        ]).
-export([registrar_details/0
         ,registrar_details/1
         ,registrar_details/2
        ]).
-export([flush_authn/0]).
-export([flush_util/0]).

-export([show_channels/0]).
-export([show_calls/0]).

-include("ecallmgr.hrl").

-spec add_fs_node(string() | binary() | atom()) -> 'ok'.
-spec add_fs_node(string() | binary() | atom(), text() | boolean()) -> 'ok'.
add_fs_node(FSNode) ->
    add_fs_node(FSNode, 'false').

add_fs_node(FSNode, AsDefault) when not is_atom(FSNode) ->
    add_fs_node(wh_util:to_atom(FSNode, 'true'), AsDefault);
add_fs_node(FSNode, AsDefault) ->
    Node = use_default(AsDefault),
    FSNodes = ecallmgr_config:get(<<"fs_nodes">>, [], Node),
    FSNodeBin = wh_util:to_binary(FSNode),
    case lists:member(FSNodeBin, FSNodes) of
        'true' -> 'ok';
        'false' ->
            io:format("adding ~s to ecallmgr.~s.fs_nodes~n", [FSNode, Node]),
            ecallmgr_config:set(<<"fs_nodes">>, [FSNodeBin | FSNodes], Node)
    end,
    ecallmgr_fs_nodes:add(FSNode).

-spec use_default(boolean() | ne_binary()) -> ne_binary().
use_default('true') -> <<"default">>;
use_default('false') -> wh_util:to_binary(node());
use_default(AsDefault) -> use_default(wh_util:is_true(AsDefault)).

-spec remove_fs_node(string() | binary() | atom()) -> 'ok'.
-spec remove_fs_node(string() | binary() | atom(), boolean() | text()) -> 'ok'.
remove_fs_node(FSNode) ->
    remove_fs_node(FSNode, 'false').

remove_fs_node(FSNode, AsDefault) when not is_atom(FSNode) ->
    remove_fs_node(wh_util:to_atom(FSNode, 'true'), AsDefault);
remove_fs_node(FSNode, AsDefault) ->
    Node = use_default(AsDefault),
    FSNodes = ecallmgr_config:get(<<"fs_nodes">>, [], Node),
    FSNodeBin = wh_util:to_binary(FSNode),
    case lists:member(FSNodeBin, FSNodes) of
        'false' -> 'ok';
        'true' ->
            io:format("removing ~s from ecallmgr.~s.fs_nodes~n", [FSNode, Node]),
            ecallmgr_config:set(<<"fs_nodes">>, lists:delete(FSNodeBin, FSNodes), Node)
    end,
    ecallmgr_fs_nodes:remove(FSNode).

-spec list_fs_nodes() -> atoms().
list_fs_nodes() -> ecallmgr_fs_nodes:connected().

-spec node_summary() -> 'no_return'.
node_summary() ->
    ecallmgr_fs_nodes:summary(),
    'no_return'.

-spec node_details() -> 'no_return'.
node_details() ->
    ecallmgr_fs_nodes:details(),
    'no_return'.

-spec node_details(text()) -> 'no_return'.
node_details(NodeName) ->
    ecallmgr_fs_nodes:details(NodeName),
    'no_return'.

-spec authz_summary() -> 'no_return'.
authz_summary() ->
    ecallmgr_fs_channels:authz_summary(),
    'no_return'.

-spec channel_summary() -> 'no_return'.
channel_summary() ->
    ecallmgr_fs_channels:summary(),
    'no_return'.

-spec channel_summary(text()) -> 'no_return'.
channel_summary(Node) ->
    ecallmgr_fs_channels:summary(Node),
    'no_return'.

-spec channel_details() -> 'no_return'.
channel_details() ->
    ecallmgr_fs_channels:details(),
    'no_return'.

-spec channel_details(text()) -> 'no_return'.
channel_details(UUID) ->
    ecallmgr_fs_channels:details(UUID),
    'no_return'.

-spec sync_channels() -> 'ok'.
sync_channels() ->
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
        ],
    'ok'.

-spec sync_channels(text()) -> 'ok'.
sync_channels(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({'p', 'l', 'fs_node'})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    'ok'.

-spec conference_summary() -> 'no_return'.
conference_summary() ->
    ecallmgr_fs_conferences:summary(),
    'no_return'.

-spec conference_summary(text()) -> 'no_return'.
conference_summary(Node) ->
    ecallmgr_fs_conferences:summary(Node),
    'no_return'.

-spec conference_details() -> 'no_return'.
conference_details() ->
    ecallmgr_fs_conferences:details(),
    'no_return'.

-spec conference_details(text()) -> 'no_return'.
conference_details(UUID) ->
    ecallmgr_fs_conferences:details(UUID),
    'no_return'.

-spec sync_conferences() -> 'ok'.
sync_conferences() ->
    _ = [ecallmgr_fs_conferences:sync_node(N)
         || N <- ecallmgr_fs_nodes:connected()
        ],
    'ok'.

-spec sync_conferences(text()) -> 'ok'.
sync_conferences(Node) ->
    N = wh_util:to_atom(Node, 'true'),
    ecallmgr_fs_conferences:sync_node(N),
    'ok'.

-spec flush_node_channels(string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    ecallmgr_fs_channels:flush_node(Node).

-spec flush_node_conferences(string() | binary() | atom()) -> 'ok'.
flush_node_conferences(Node) ->
    ecallmgr_fs_conferences:flush_node(Node).

-spec flush_registrar() -> 'ok'.
flush_registrar() ->
    ecallmgr_registrar:flush().

-spec flush_registrar(text()) -> 'ok'.
flush_registrar(Realm) ->
    ecallmgr_registrar:flush(Realm).

-spec flush_registrar(text(), text()) -> 'ok'.
flush_registrar(Username, Realm) ->
    ecallmgr_registrar:flush(Username, Realm).

-spec registrar_summary() -> 'no_return'.
registrar_summary() ->
    ecallmgr_registrar:summary(),
    'no_return'.

-spec registrar_summary(text()) -> 'no_return'.
registrar_summary(Realm) ->
    ecallmgr_registrar:summary(Realm),
    'no_return'.

-spec registrar_details() -> 'no_return'.
registrar_details() ->
    ecallmgr_registrar:details(),
    'no_return'.

-spec registrar_details(text()) -> 'no_return'.
registrar_details(Realm) ->
    ecallmgr_registrar:details(Realm),
    'no_return'.

-spec registrar_details(text(), text()) -> 'no_return'.
registrar_details(Username, Realm) ->
    ecallmgr_registrar:details(Username, Realm),
    'no_return'.

-spec flush_authn() -> 'ok'.
flush_authn() ->
    wh_cache:flush_local(?ECALLMGR_AUTH_CACHE).

-spec flush_util() -> 'ok'.
flush_util() ->
    wh_cache:flush_local(?ECALLMGR_UTIL_CACHE).

-spec show_channels() -> 'no_return'.
show_channels() ->
    io:format("This function is depreciated, please use channel_summary or channel_detail~n"),
    'no_return'.

-spec show_calls() -> 'no_return'.
show_calls() ->
    io:format("This function is depreciated, please use channel_summary or channel_detail~n"),
    'no_return'.
