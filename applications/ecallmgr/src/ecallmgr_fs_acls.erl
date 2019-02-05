%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_acls).

-export([get/0, get/1
        ,system/0, system/1
        ]).

-compile({'no_auto_import', [get/1]}).

-include("ecallmgr.hrl").

-define(REQUEST_TIMEOUT
       ,kapps_config:get_integer(?APP_NAME
                                ,<<"acl_request_timeout_ms">>
                                ,2 * ?MILLISECONDS_IN_SECOND
                                )
       ).
-define(REQUEST_TIMEOUT_FUDGE
       ,kapps_config:get_integer(?APP_NAME
                                ,<<"acl_request_timeout_fudge_ms">>
                                ,100
                                )
       ).
-define(IP_REGEX, <<"^(\\d{1,3}\\\.\\d{1,3}\\\.\\d{1,3}\\\.\\d{1,3}).*">>).
-define(ACL_RESULT(IP, ACL), {'acl', IP, ACL}).

-type acls() :: kz_json:object().

%%------------------------------------------------------------------------------
%% @doc Fetches the ACLs
%% 1. from system_config
%% 2. auth-by-IP devices
%% 3. local resources
%% 4. global resources
%%
%% @end
%%------------------------------------------------------------------------------
-spec get() -> acls().
get() ->
    Node = kz_term:to_binary(node()),
    get(Node).

-spec get(atom() | kz_term:ne_binary()) -> acls().
get(Node) ->
    Routines = [fun offnet_resources/1
               ,fun local_resources/1
               ,fun sip_auth_ips/1
               ],
    PidRefs = [kz_util:spawn_monitor(fun erlang:apply/2, [F, [self()]]) || F <- Routines],
    lager:debug("collecting ACLs in ~p", [PidRefs]),
    collect(system_config_acls(Node), PidRefs).

%%------------------------------------------------------------------------------
%% @doc Fetches just the system_config ACLs
%% @end
%%------------------------------------------------------------------------------
-spec system() -> acls() | {'error', any()}.
system() ->
    system(kz_term:to_binary(node())).

-spec system(atom() | kz_term:ne_binary()) -> acls() | {'error', any()}.
system(Node) ->
    kapps_config:fetch_current(?APP_NAME, <<"acls">>, kz_json:new(), Node).

-spec collect(kz_json:object(), kz_term:pid_refs()) ->
                     kz_json:object().
collect(ACLs, PidRefs) ->
    collect(ACLs, PidRefs, request_timeout()).

-spec request_timeout() -> pos_integer().
request_timeout() ->
    ?REQUEST_TIMEOUT + ?REQUEST_TIMEOUT_FUDGE.

-spec collect(kz_json:object(), kz_term:pid_refs(), timeout()) ->
                     kz_json:object().
collect(ACLs, [], _Timeout) ->
    lager:debug("acls built with ~p ms to spare", [_Timeout]),
    ACLs;
collect(ACLs, _PidRefs, Timeout) when Timeout < 0 ->
    lager:debug("timed out waiting for ACLs, returning what we got"),
    ACLs;
collect(ACLs, PidRefs, Timeout) ->
    Start = os:timestamp(),
    receive
        ?ACL_RESULT(IP, ACL) ->
            lager:info("adding acl for '~s' to ~s", [IP, kz_json:get_value(<<"network-list-name">>, ACL)]),
            collect(kz_json:set_value(IP, ACL, ACLs), PidRefs, kz_time:decr_timeout(Timeout, Start));
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            case lists:keytake(Pid, 1, PidRefs) of
                'false' ->
                    collect(ACLs, PidRefs, kz_time:decr_timeout(Timeout, Start));
                {'value', {Pid, Ref}, NewPidRefs} ->
                    collect(ACLs, NewPidRefs, kz_time:decr_timeout(Timeout, Start))
            end
    after Timeout ->
            lager:debug("timed out collecting acls, working with what we have"),
            ACLs
    end.

-spec system_config_acls(atom() | kz_term:ne_binary()) -> acls().
system_config_acls(Node) ->
    case kapps_config:fetch_current(?APP_NAME, <<"acls">>, kz_json:new(), Node) of
        {'error', Error} ->
            lager:warning("error getting system acls : ~p", [Error]),
            kz_json:new();
        JObj -> JObj
    end.

-spec sip_auth_ips(pid()) -> 'ok'.
sip_auth_ips(Collector) ->
    ViewOptions = [],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'error', _R} ->
            lager:info("unable to get view results for auth-by-ip devices: ~p", [_R]);
        {'ok', JObjs} ->
            {RawIPs, RawHosts} = lists:foldl(fun needs_resolving/2, {[], []}, JObjs),
            _ = [handle_sip_auth_result(Collector, JObj, IPs) || {IPs, JObj} <- RawIPs],
            PidRefs = [kz_util:spawn_monitor(fun resolve_hostname/4 ,[Collector
                                                                     ,Host
                                                                     ,JObj
                                                                     ,fun handle_sip_auth_result/3
                                                                     ])
                       || {Host, JObj} <- RawHosts
                      ],
            wait_for_pid_refs(PidRefs)
    end.

-spec needs_resolving(kz_json:object(), {list(), list()}) -> {list(), list()}.
needs_resolving(JObj, {IPs, ToResolve}) ->
    IP = kz_json:get_value(<<"key">>, JObj),
    case kz_network_utils:is_ipv4(IP) of
        'true' -> {[{[IP], JObj}|IPs], ToResolve};
        'false' -> {IPs, [{IP, JObj} | ToResolve]}
    end.

-spec wait_for_pid_refs(kz_term:pid_refs()) -> 'ok'.
wait_for_pid_refs(PidRefs) ->
    wait_for_pid_refs(PidRefs, ?REQUEST_TIMEOUT).
wait_for_pid_refs([], _Timeout) -> 'ok';
wait_for_pid_refs(_PidRefs, Timeout) when Timeout < 0 -> 'ok';
wait_for_pid_refs(PidRefs, Timeout) ->
    Start = os:timestamp(),
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            case lists:keytake(Pid, 1, PidRefs) of
                'false' -> wait_for_pid_refs(PidRefs, kz_time:decr_timeout(Timeout, Start));
                {'value', {Pid, Ref}, NewPidRefs} ->
                    wait_for_pid_refs(NewPidRefs, kz_time:decr_timeout(Timeout, Start))
            end
    after Timeout ->
            lager:debug("timed out waiting for pid refs: ~p", [PidRefs])
    end.

-spec resolve_hostname(pid(), kz_term:ne_binary(), kz_json:object(), fun()) -> 'ok'.
resolve_hostname(Collector, ResolveMe, JObj, ACLBuilderFun) ->
    lager:debug("attempting to resolve '~s'", [ResolveMe]),
    StrippedHost = hd(binary:split(ResolveMe, <<";">>)),
    case (not kz_network_utils:is_ipv4(StrippedHost))
        andalso kz_network_utils:resolve(StrippedHost)
    of
        'false' ->
            maybe_capture_ip(Collector, ResolveMe, JObj, ACLBuilderFun);
        [] ->
            lager:debug("no IPs returned, checking for raw IP"),
            maybe_capture_ip(Collector, ResolveMe, JObj, ACLBuilderFun);
        IPs ->
            ACLBuilderFun(Collector, JObj, IPs),
            lager:debug("resolved '~s' (~s) for ~p: '~s'", [StrippedHost, ResolveMe, Collector, kz_binary:join(IPs, <<"','">>)])
    end.

-spec maybe_capture_ip(pid(), kz_term:ne_binary(), kz_json:object(), fun()) -> 'ok'.
maybe_capture_ip(Collector, CaptureMe, JObj, ACLBuilderFun) ->
    case re:run(CaptureMe, ?IP_REGEX, [{'capture', 'all', 'binary'}]) of
        {'match', [_All, IP]} ->
            ACLBuilderFun(Collector, JObj, [IP]),
            lager:debug("captured '~s' from ~s", [IP, CaptureMe]);
        'nomatch' ->
            lager:debug("failed to find IP at start of '~s'", [CaptureMe])
    end.

-spec handle_sip_auth_result(pid(), kz_json:object(), kz_term:ne_binaries()) -> 'ok'.
handle_sip_auth_result(Collector, JObj, IPs) ->
    AccountId = kz_json:get_value([<<"value">>, <<"account_id">>], JObj),
    AuthorizingId = kz_doc:id(JObj),
    AuthorizingType = kz_json:get_value([<<"value">>, <<"authorizing_type">>], JObj),
    add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, IPs).

-spec local_resources(pid()) -> 'ok'.
local_resources(Collector) ->
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get view results for local active resources: ~p", [_R]);
        {'ok', JObjs} ->
            handle_resource_results(Collector, JObjs)
    end.

-spec offnet_resources(pid()) -> 'ok'.
offnet_resources(Collector) ->
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(?KZ_OFFNET_DB, <<"resources/listing_active_by_weight">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get view results for offnet active resources: ~p", [_R]);
        {'ok', JObjs} ->
            handle_resource_results(Collector, JObjs)
    end.

-spec handle_resource_results(pid(), kz_json:objects()) -> 'ok'.
handle_resource_results(Collector, JObjs) ->
    _ = [handle_resource_result(Collector, JObj) || JObj <- JObjs],
    'ok'.

-spec handle_resource_result(pid(), kz_json:object()) -> 'ok'.
handle_resource_result(Collector, JObj) ->
    Doc = kz_json:get_json_value(<<"doc">>, JObj),
    InboundPidRefs = resource_inbound_ips(Collector, Doc),
    ServerPidRefs = resource_server_ips(Collector, Doc),
    wait_for_pid_refs(InboundPidRefs ++ ServerPidRefs).

-spec handle_resource_result(pid(), kz_json:object(), kz_term:ne_binaries()) -> 'ok'.
handle_resource_result(Collector, JObj, IPs) ->
    AuthorizingId = kz_doc:id(JObj),
    add_trusted_objects(Collector, 'undefined', AuthorizingId, <<"resource">>, IPs).

-spec resource_inbound_ips(pid(), kz_json:object()) -> kz_term:pid_refs().
resource_inbound_ips(Collector, JObj) ->
    [kz_util:spawn_monitor(fun resolve_hostname/4, [Collector
                                                   ,IP
                                                   ,JObj
                                                   ,fun handle_resource_result/3
                                                   ])
     || IP <- kz_json:get_value(<<"inbound_ips">>, JObj, [])
    ].

-spec resource_server_ips(pid(), kz_json:object()) -> kz_term:pid_refs().
resource_server_ips(Collector, JObj) ->
    [kz_util:spawn_monitor(fun resolve_hostname/4, [Collector
                                                   ,kz_json:get_value(<<"server">>, Gateway)
                                                   ,JObj
                                                   ,fun handle_resource_result/3
                                                   ])
     || Gateway <- kz_json:get_value(<<"gateways">>, JObj, []),
        kz_json:is_true(<<"enabled">>, Gateway, 'false')
    ].

-spec add_trusted_objects(pid(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
add_trusted_objects(_Collector, _AccountId, _AuthorizingId, _AuthorizingType, []) -> 'ok';
add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, [IP|IPs]) ->
    Props = kz_json:from_list(
              [{<<"type">>, <<"allow">>}
              ,{<<"network-list-name">>, <<"trusted">>}
              ,{<<"cidr">>, <<IP/binary, "/32">>}
              ,{<<"account_id">>, AccountId}
              ,{<<"authorizing_id">>, AuthorizingId}
              ,{<<"authorizing_type">>, AuthorizingType}
              ]),
    Collector ! ?ACL_RESULT(IP, Props),
    add_trusted_objects(Collector, AccountId, AuthorizingId, AuthorizingType, IPs).
