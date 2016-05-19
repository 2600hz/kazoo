-module(kz_global).

-export([name/1
        ,name_pos/0
        ,pid/1
        ,zone/1
        ,server/1
        ,state/1
        ,is_local/1
        ,node/1

        ,all_names/1
        ,all_globals_by_pid/2

        ,from_jobj/2
        ,new_local/4
        ,update_with_pid_ref/2, update_with_pid_ref/3
        ]).

-export_type([global/0
             ,globals/0
             ,name/0
             ]).

-include("kazoo_globals.hrl").

-record(kz_global, {node = node() :: atom() | '_'
                   ,zone :: atom() | '_'
                   ,pid :: api_pid() | '_'
                   ,server :: any() | '_'
                   ,name :: name() | '$1' | '_'
                   ,monitor :: api_reference() | '_'
                   ,state = 'none' :: kapi_globals:state() | '_'
                   }).

-type global() :: #kz_global{}.
-type globals() :: [global()].

-type name() :: term().
-type names() :: [name()].

from_jobj(JObj, Zone) ->
    Node = kz_api:node(JObj),
    #kz_global{node=kz_util:to_atom(Node, 'true')
              ,zone=Zone
              ,server = kz_api:server_id(JObj)
              ,name = kapi_globals:name(JObj)
              ,state = kapi_globals:state(JObj)
              }.

-spec new_local(name(), pid(), atom(), ne_binary()) -> global().
new_local(Name, Pid, Zone, Queue) ->
    #kz_global{node = node()
              ,zone = Zone
              ,server = Queue
              ,pid = Pid
              ,monitor = erlang:monitor('process', Pid)
              ,name = Name
              ,state='local'
              }.

-spec update_with_pid_ref(global(), pid_ref()) -> global().
-spec update_with_pid_ref(global(), pid(), reference()) -> global().
update_with_pid_ref(Global, {Pid, Ref}) ->
    update_with_pid_ref(Global, Pid, Ref).

update_with_pid_ref(Global, Pid, Ref)
  when is_pid(Pid)
       andalso is_reference(Ref)
       ->
    Global#kz_global{monitor=Ref
                    ,pid=Pid
                    }.

-spec all_names(ets:tab()) -> names().
all_names(Table) ->
    MatchSpec = [{#kz_global{name = '$1', _ = '_'} ,[],['$1']}],
    ets:select(Table, MatchSpec).

-spec all_globals_by_pid(ets:tab(), pid()) -> globals().
all_globals_by_pid(Table, Pid) ->
    MatchSpec = [{#kz_global{pid = Pid, _ = '_'} ,[],['$_']}],
    ets:select(Table, MatchSpec).

name(#kz_global{name=Name}) ->
    Name.

name_pos() ->
    #kz_global.name.

pid(#kz_global{pid=Pid}) ->
    Pid.

zone(#kz_global{zone=Zone}) ->
    Zone.

server(#kz_global{server=Queue}) ->
    Queue.

node(#kz_global{node=Node}) ->
    Node.

state(#kz_global{state=State}) ->
    State.

is_local(#kz_global{state='local'}) -> 'true';
is_local(#kz_global{}) -> 'false'.
