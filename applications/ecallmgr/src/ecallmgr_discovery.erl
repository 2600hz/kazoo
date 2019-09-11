%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_discovery).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([discover/0]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type state() :: kz_time:start_time().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%%
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state(), pos_integer()}.
init([]) ->
    lager:info("starting discovery"),
    {'ok', kz_time:start_time(), ?MILLISECONDS_IN_SECOND}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, Startup) ->
    {'reply', {'error', 'not_implemented'}, next_timeout(kz_time:elapsed_s(Startup))}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('discovery', Startup) ->
    lager:warning("starting discovery"),
    _ = sbc_discovery(),
    {'noreply', Startup, next_timeout(kz_time:elapsed_s(Startup))};
handle_cast(_Msg, Startup) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', Startup, next_timeout(kz_time:elapsed_s(Startup))}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', Startup) ->
    _ = sbc_discovery(),
    {'noreply', Startup, next_timeout(kz_time:elapsed_s(Startup))};
handle_info({'bgok', _Id, _Result}, Startup) ->
    lager:info("background job ~s: ~s", [_Id, _Result]),
    {'noreply', Startup, next_timeout(kz_time:elapsed_s(Startup))};
handle_info(_Msg, Startup) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', Startup, next_timeout(kz_time:elapsed_s(Startup))}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, _Startup) ->
    lager:debug("ecallmgr discovery terminating after ~ps: ~p", [kz_time:elapsed_s(_Startup), Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, Startup, _Extra) ->
    {'ok', Startup}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec next_timeout(integer()) -> integer().
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 5 -> ?MILLISECONDS_IN_SECOND;
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 10 -> ?MILLISECONDS_IN_SECOND * 3;
next_timeout(Elapsed)
  when Elapsed < ?SECONDS_IN_MINUTE * 20 -> ?MILLISECONDS_IN_SECOND * 5;
next_timeout(_Elapsed) -> ?MILLISECONDS_IN_MINUTE.

sbc_acl_filter({_K, V}) ->
    kz_json:get_ne_binary_value(<<"network-list-name">>, V) =:= <<"authoritative">>.

sbc_cidr(_, JObj, Acc) ->
    [{kz_json:get_value(<<"cidr">>, JObj), kz_json:get_value(<<"ports">>, JObj, [])} | Acc].

sbc_cidrs(ACLs) ->
    SBCs = kz_json:filter(fun sbc_acl_filter/1, ACLs),
    lists:flatten(kz_json:foldl(fun sbc_cidr/3, [], SBCs)).

sbc_address_foldl(_, JObj, Acc) ->
    IP = kz_json:get_ne_binary_value(<<"address">>, JObj),
    Port = kz_json:get_integer_value(<<"port">>, JObj, 0),
    case props:get_value(IP, Acc, []) of
        [] -> [{IP, [Port]} | Acc];
        Ports -> props:set_value(IP, lists:usort([Port | Ports]), Acc)
    end.

sbc_addresses(#kz_node{roles=Roles}) ->
    Listeners = kz_json:get_json_value(<<"Listeners">>, props:get_value(<<"Proxy">>, Roles)),
    kz_json:foldl(fun sbc_address_foldl/3, [], Listeners).

sbc_node(#kz_node{node=Name}=Node) ->
    {kz_term:to_binary(Name), sbc_addresses(Node)}.

sbc_verify_ip({IP, Ports}, CIDRs) ->
    lists:any(fun({CIDR, CIDRPorts}) when is_list(CIDR) ->
                      lists:all(fun(CIDR_IP) -> kz_network_utils:verify_cidr(IP, CIDR_IP) end, CIDR)
                          andalso Ports -- CIDRPorts  =:= []
                          andalso CIDRPorts -- Ports =:= [];
                 ({CIDR, CIDRPorts}) ->
                      kz_network_utils:verify_cidr(IP, CIDR)
                          andalso CIDRPorts -- Ports =:= []
                          andalso Ports -- CIDRPorts  =:= []
              end, CIDRs).

sbc_discover({Node, IPs}, CIDRs, Acc) ->
    case lists:filter(fun(IP) -> not sbc_verify_ip(IP, CIDRs) end, IPs) of
        [] -> Acc;
        Filtered -> [{Node, Filtered} | Acc]
    end.

-spec filter_acls(kz_json:object()) -> kz_json:object().
filter_acls(ACLs) ->
    kz_json:filter(fun filter_acls_fun/1, ACLs).

-spec filter_acls_fun({kz_json:path(), kz_json:json_term()}) -> boolean().
filter_acls_fun({_Name, ACL}) ->
    kz_json:get_ne_binary_value(<<"authorizing_type">>, ACL) =:= 'undefined'.

sbc_acl(IPs) ->
    CIDRs = [kz_network_utils:to_cidr(IP) || {IP, _} <- IPs],

    kz_json:from_list([{<<"type">>, <<"allow">>}
                      ,{<<"network-list-name">>, ?FS_SBC_ACL_LIST}
                      ,{<<"cidr">>, CIDRs}
                      ,{<<"ports">>, lists:usort(lists:flatten([Ports || {_, Ports} <- IPs]))}
                      ]).

sbc_acls(Nodes) ->
    [{Node, sbc_acl(IPs)} || {Node, IPs} <- Nodes].

-spec sbc_discovery() -> any().
sbc_discovery() ->
    sbc_discovery(<<"default">>).

-spec sbc_discovery(kz_term:ne_binary()) -> any().
sbc_discovery(Node) ->
    case ecallmgr_fs_acls:system(Node) of
        {'error', Error} -> lager:warning("error fetching current acls - ~p", [Error]);
        CurrentACLs -> sbc_discovery(Node, CurrentACLs)
    end.

-spec sbc_discovery(kz_term:ne_binary(), kz_json:object()) -> any().
sbc_discovery(ConfigNode, CurrentACLs) ->
    ACLs = filter_acls(CurrentACLs),
    CIDRs = sbc_cidrs(ACLs),
    Nodes = [sbc_node(Node) || Node <- kz_nodes:with_role(<<"Proxy">>, 'true')],
    case lists:foldl(fun(A, C) -> sbc_discover(A, CIDRs, C) end, [], Nodes) of
        [] -> 'ok';
        Updates ->
            Names = lists:usort(lists:map(fun({Node, _}) -> Node end, Updates)),
            lager:debug("adding authoritative acls for ~s", [kz_binary:join(Names)]),
            ToUpdate = lists:filter(fun({Node, _IPs}) -> lists:member(Node, Names) end , Nodes),
            SBCACLs = sbc_acls(ToUpdate),
            NewAcls = kz_json:set_values(SBCACLs, CurrentACLs),
            _ = kapps_config:set_node(?APP_NAME, <<"acls">>, NewAcls, ConfigNode),
            ecallmgr_maintenance:reload_acls()
    end.

-spec discover() -> 'ok'.
discover() ->
    gen_server:cast(?MODULE, 'discovery').
