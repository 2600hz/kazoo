-module(pqc_cb_ips).
-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

%% Crossbar API requests
-export([list_ips/1
        ,assign_ips/3
        ,remove_ip/3
        ,fetch_ip/3
        ,assign_ip/3
        ,fetch_hosts/1
        ,fetch_zones/1
        ,fetch_assigned/2
        ,create_ip/2
        ,delete_ip/2
        ]).

-export([ips_url/0, ips_url/1]).

-export([cleanup/0, cleanup/1
        ,seq/0
        ]).

-export_type([dedicated/0]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").

-record('dedicated', {ip :: api_ne_binary()
                     ,host :: api_ne_binary()
                     ,zone :: api_ne_binary()
                     }).
-define(DEDICATED(IP, Host, Zone)
       ,#dedicated{ip=IP, host=Host, zone=Zone}
       ).
-type dedicated() :: #dedicated{}.

-spec ips_url() -> string().
-spec ips_url(pqc_cb_accounts:account_id()) -> string().
ips_url() ->
    string:join([pqc_cb_api:v2_base_url(), "ips"], "/").

ips_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ips"], "/").

-spec ip_url(text()) -> string().
-spec ip_url(pqc_cb_accounts:account_id(), text()) -> string().
ip_url(IP) ->
    string:join([pqc_cb_api:v2_base_url(), "ips", kz_term:to_list(IP)], "/").

ip_url(AccountId, IP) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ips", kz_term:to_list(IP)], "/").

-spec list_ips(pqc_cb_api:state()) ->
                      {'ok', kz_json:objects()} |
                      {'error', 'not_found'}.
list_ips(API) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ips_url()
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("listing IPs errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            ?DEBUG("listing IPs: ~s", [Response]),
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec assign_ips(pqc_cb_api:state(), pqc_cb_accounts:account_id(), [dedicated()]) ->
                        {'ok', kz_json:objects()} |
                        {'error', 'not_found'}.
assign_ips(_API, 'undefined', _Dedicateds) ->
    {'error', 'not_found'};
assign_ips(API, AccountId, Dedicateds) ->
    IPs = [IP || ?DEDICATED(IP, _, _) <- Dedicateds],
    Envelope = pqc_cb_api:create_envelope(IPs),

    case pqc_cb_api:make_request([200]
                                ,fun kz_http:post/3
                                ,ips_url(AccountId)
                                ,pqc_cb_api:request_headers(API)
                                ,kz_json:encode(Envelope)
                                )
    of
        {'error', _E} ->
            ?DEBUG("assigning IPs errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.


-spec remove_ip(pqc_cb_api:state(), pqc_cb_accounts:account_id(), dedicated()) ->
                       {'ok', kz_json:object()} |
                       {'error', 'not_found'}.
remove_ip(_API, 'undefined', _Dedicated) ->
    {'error', 'not_found'};
remove_ip(API, AccountId, ?DEDICATED(IP, _, _)) ->
    case pqc_cb_api:make_request([200, 404]
                                ,fun kz_http:delete/2
                                ,ip_url(AccountId, IP)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("removing IP errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_json_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_ip(pqc_cb_api:state(), pqc_cb_accounts:account_id(), dedicated()) ->
                      {'ok', kz_json:object()} |
                      {'error', 'not_found'}.
fetch_ip(_API, 'undefined', _Dedicated) ->
    {'error', 'not_found'};
fetch_ip(API, AccountId, ?DEDICATED(IP, _, _)) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url(AccountId, IP)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetching IP errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_json_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec assign_ip(pqc_cb_api:state(), pqc_cb_accounts:account_id(), dedicated()) ->
                       {'ok', kz_json:object()} |
                       {'error', 'not_found'}.
assign_ip(_API, 'undefined', _Dedicated) ->
    {'error', 'not_found'};
assign_ip(API, AccountId, ?DEDICATED(IP, _, _)) ->
    Envelope = pqc_cb_api:create_envelope(kz_json:new()),
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:post/3
                                ,ip_url(AccountId, IP)
                                ,pqc_cb_api:request_headers(API)
                                ,kz_json:encode(Envelope)
                                )
    of
        {'error', _E} ->
            ?DEBUG("assigning IP errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_json_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_hosts(pqc_cb_api:state()) ->
                         {'ok', ne_binaries()} |
                         {'error', 'not_found'}.
fetch_hosts(API) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url("hosts")
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetch hosts errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_zones(pqc_cb_api:state()) ->
                         {'ok', ne_binaries()} |
                         {'error', 'not_found'}.
fetch_zones(API) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url("zones")
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetch zones errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_assigned(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                            {'ok', kz_json:objects()} |
                            {'error', 'not_found'}.
fetch_assigned(_API, 'undefined') ->
    {'error', 'not_found'};
fetch_assigned(API, AccountId) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url(AccountId, "assigned")
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetch zones errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec create_ip(pqc_cb_api:state(), dedicated()) ->
                       {'ok', kz_json:object()} |
                       {'error', 'not_found' | 'conflict'}.
create_ip(API, ?DEDICATED(IP, Host, Zone)) ->
    Data = kz_json:from_list([{<<"ip">>, IP}
                             ,{<<"host">>, Host}
                             ,{<<"zone">>, Zone}
                             ]),
    Envelope = pqc_cb_api:create_envelope(Data),
    case pqc_cb_api:make_request([201, 409]
                                ,fun kz_http:put/3
                                ,ips_url()
                                ,pqc_cb_api:request_headers(API)
                                ,kz_json:encode(Envelope)
                                )
    of
        {'error', _E} ->
            ?DEBUG("create ip errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            JObj = kz_json:decode(Response),
            case kz_json:get_integer_value(<<"error">>, JObj) of
                'undefined' ->
                    {'ok', kz_json:get_value([<<"data">>, <<"_read_only">>], JObj)};
                409 ->
                    {'error', 'conflict'}
            end
    end.

-spec delete_ip(pqc_cb_api:state(), dedicated()) ->
                       {'ok', kz_json:object()} |
                       {'error', 'not_found'}.
delete_ip(API, ?DEDICATED(IP, _Host, _Zone)) ->
    case pqc_cb_api:make_request([200, 404]
                                ,fun kz_http:delete/2
                                ,ip_url(IP)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("delete ip errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            JObj = kz_json:decode(Response),
            case kz_json:get_integer_value(<<"error">>, JObj) of
                404 -> {'error', 'not_found'};
                _ -> {'ok', kz_json:get_list_value(<<"data">>, JObj)}
            end
    end.

-define(ACCOUNT_NAMES, [<<"account_for_ips">>]).
-define(DEDICATED_IPS, [?DEDICATED(<<"1.2.3.4">>, <<"a.host.com">>, <<"zone-1">>)]).

-spec cleanup() -> any().
-spec cleanup(pqc_cb_api:state()) -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),

    cleanup(pqc_cb_api:authenticate()).

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = [delete_ip(API, Dedicated) || Dedicated <- ?DEDICATED_IPS],

    pqc_cb_api:cleanup(API).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_ips', 'cb_accounts']
        ],
    ?INFO("INIT FINISHED").

-spec seq() -> 'ok'.
seq() ->
    init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    IP = ?DEDICATED(<<"1.2.3.4">>, <<"a.host.com">>, <<"zone-1">>),

    try
        {'ok', Created} = create_ip(API, IP),
        ?INFO("created ip ~p", [Created]),

        AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
        AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
        ?INFO("created account ~s", [AccountId]),

        {'ok', IPs} = list_ips(API),
        ?INFO("ips available: ~p", [IPs]),

        {'ok', Assigned} = assign_ip(API, AccountId, IP),
        ?INFO("assigned ~p: ~p", [IP, Assigned]),

        {'ok', Fetched} = fetch_ip(API, AccountId, IP),
        ?INFO("fetched ~p: ~p", [IP, Fetched]),

        {'ok', Hosts} = fetch_hosts(API),
        ?INFO("hosts: ~p", [Hosts]),

        {'ok', Zones} = fetch_zones(API),
        ?INFO("zones: ~p", [Zones]),

        {'ok', AssignedIPs} = fetch_assigned(API, AccountId),
        ?INFO("assigned ips: ~p", [AssignedIPs])

    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            ?INFO("failed ~s: ~p", [_E, _R]),
            [?INFO("st: ~p", [S]) || S <- ST]
    after
        pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
        delete_ip(API, IP),
        pqc_cb_api:cleanup(API)
    end,
    ?INFO("seq finished running: ~p", [API]),
    io:format("seq finished running: ~p", [API]).

-spec command(any()) -> proper_types:type().
command(Model) ->
    command(Model, pqc_kazoo_model:has_accounts(Model)).

command(Model, 'false') ->
    AccountName = account_name(),
    pqc_cb_accounts:command(Model, AccountName);
command(Model, 'true') ->
    API = pqc_kazoo_model:api(Model),

    AccountName = account_name(),
    AccountId = pqc_cb_accounts:symbolic_account_id(Model, AccountName),

    oneof([pqc_cb_accounts:command(Model, AccountName)
          ,{'call', ?MODULE, 'list_ips', [API]}
          ,{'call', ?MODULE, 'assign_ips', [API, AccountId, ips()]}
          ,{'call', ?MODULE, 'remove_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'fetch_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'assign_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'fetch_hosts', [API]}
          ,{'call', ?MODULE, 'fetch_zones', [API]}
          ,{'call', ?MODULE, 'fetch_assigned', [API, AccountId]}
          ,{'call', ?MODULE, 'create_ip', [API, ip()]}
          ,{'call', ?MODULE, 'delete_ip', [API, ip()]}
          ]
         ).

account_name() ->
    oneof(?ACCOUNT_NAMES).

ip() ->
    oneof(?DEDICATED_IPS).

ips() ->
    non_empty(ip()).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model, APIResp, {'call', _, 'create_account', _Args}=Call) ->
    pqc_cb_accounts:next_state(Model, APIResp, Call);
next_state(Model, _APIResp, {'call', ?MODULE, 'list_ips', [_API]}) ->
    Model;
next_state(Model, _APIResp, {'call', ?MODULE, 'assign_ips', [_API, AccountId, Dedicateds]}) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun do_dedicated_ips_exist/2, [Dedicateds]}
                           ,{fun are_dedicated_ips_unassigned/2, [Dedicateds]}
                           ,{fun assign_dedicated_ips/3, [AccountId, Dedicateds]}
                           ]
                          );
next_state(Model, _APIResp, {'call', ?MODULE, 'remove_ip', [_API, AccountId, ?DEDICATED(IP, _, _)]}) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:does_ip_exist/2, [IP]}
                           ,{fun pqc_kazoo_model:is_ip_assigned/3, [AccountId, IP]}
                           ,{fun pqc_kazoo_model:unassign_dedicated_ip/2, [IP]}
                           ]
                          );
next_state(Model, _APIResp, {'call', ?MODULE, 'fetch_ip', [_API, _AccountId, _Dedicated]}) ->
    Model;
next_state(Model, _APIResp, {'call', ?MODULE, 'assign_ip', [_API, AccountId, ?DEDICATED(IP, _, _)]}) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:does_ip_exist/2, [IP]}
                           ,{fun pqc_kazoo_model:is_ip_unassigned/2, [IP]}
                           ,{fun pqc_kazoo_model:assign_dedicated_ip/3, [AccountId, IP]}
                           ]
                          );
next_state(Model, _APIResp, {'call', ?MODULE, 'fetch_hosts', [_API]}) ->
    Model;
next_state(Model, _APIResp, {'call', ?MODULE, 'fetch_zones', [_API]}) ->
    Model;
next_state(Model, _APIResp, {'call', ?MODULE, 'fetch_assigned', [_API, _AccountId]}) ->
    Model;
next_state(Model, _APIResp, {'call', ?MODULE, 'create_ip', [_API, ?DEDICATED(IP, Host, Zone)]}) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_ip_missing/2, [IP]}
                           ,{fun pqc_kazoo_model:add_dedicated_ip/4, [IP, Host, Zone]}
                           ]
                          );
next_state(Model, _APIResp, {'call', ?MODULE, 'delete_ip', [_API, ?DEDICATED(IP, _Host, _Zone)]}) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_ip_exist/2, [IP]}
                           ,{fun pqc_kazoo_model:remove_dedicated_ip/2, [IP]}
                           ]
                          ).

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model, {'call', _, 'create_account', _Args}=Call, APIResult) ->
    pqc_cb_accounts:postcondition(Model, Call, APIResult);
postcondition(Model, {'call', ?MODULE, 'list_ips', [_API]}, {'ok', []}) ->
    [] =:= pqc_kazoo_model:dedicated_ips(Model);
postcondition(Model, {'call', ?MODULE, 'list_ips', [_API]}, {'ok', ListedIPs}) ->
    lists:all(fun({IP, IPInfo}) ->
                      is_ip_listed(IP, IPInfo, ListedIPs)
              end
             ,pqc_kazoo_model:dedicated_ips(Model)
             );
postcondition(Model, {'call', ?MODULE, 'list_ips', [_API]}, {'error', 'not_found'}) ->
    [] =:= pqc_kazoo_model:dedicated_ips(Model);

postcondition(Model, {'call', ?MODULE, 'assign_ips', [_API, AccountId, Dedicateds]}, {'ok', ListedIPs}) ->
    lists:all(fun({IP, IPInfo}) ->
                      not is_ip_listed(IP, IPInfo, ListedIPs)
              end
             ,pqc_kazoo_model:account_ips(Model, AccountId)
             )
        andalso all_requested_are_listed(AccountId, Dedicateds, ListedIPs);
postcondition(_Model, {'call', ?MODULE, 'assign_ips', [_API, _AccountId, _Dedicateds]}, {'error', 'not_found'}) -> 'true';
postcondition(Model, {'call', ?MODULE, 'remove_ip', [_API, AccountId, ?DEDICATED(IP, Host, Zone)]}, {'ok', RemovedIP}) ->
    pqc_kazoo_model:is_ip_assigned(Model, AccountId, IP)
        andalso IP =:= kz_json:get_ne_binary_value(<<"ip">>, RemovedIP)
        andalso Host =:= kz_json:get_ne_binary_value(<<"host">>, RemovedIP)
        andalso Zone =:= kz_json:get_ne_binary_value(<<"zone">>, RemovedIP)
        andalso 'true' =:= kz_json:is_true([<<"_read_only">>, <<"deleted">>], RemovedIP);
postcondition(Model, {'call', ?MODULE, 'remove_ip', [_API, AccountId, ?DEDICATED(IP, _Host, _Zone)]}, {'error', 'not_found'}) ->
    not pqc_kazoo_model:is_ip_assigned(Model, AccountId, IP);
postcondition(Model, {'call', ?MODULE, 'fetch_ip', [_API, AccountId, ?DEDICATED(IP, _Host, _Zone)=Dedicated]}, {'ok', FetchedIP}) ->
    pqc_kazoo_model:is_ip_assigned(Model, AccountId, IP)
        andalso is_assigned(AccountId, Dedicated, FetchedIP);
postcondition(Model, {'call', ?MODULE, 'fetch_ip', [_API, AccountId, ?DEDICATED(IP, _Host, _Zone)]}, {'error', 'not_found'}) ->
    not pqc_kazoo_model:is_ip_assigned(Model, AccountId, IP);
postcondition(Model, {'call', ?MODULE, 'assign_ip', [_API, AccountId, ?DEDICATED(_, _, _)=Dedicated]}, {'ok', AssignedIP}) ->
    lists:all(fun({IP, IPInfo}) ->
                      not is_ip_listed(IP, IPInfo, [AssignedIP])
              end
             ,pqc_kazoo_model:account_ips(Model, AccountId)
             )
        andalso all_requested_are_listed(AccountId, [Dedicated], [AssignedIP]);
postcondition(_Model, {'call', ?MODULE, 'assign_ip', [_API, _AccountId, _Dedicated]}, {'error', 'not_found'}) -> 'true';
postcondition(Model, {'call', ?MODULE, 'fetch_zones', [_API]}, {'ok', Zones}) ->
    lists:usort(Zones) =:= lists:usort(pqc_kazoo_model:dedicated_zones(Model));
postcondition(Model, {'call', ?MODULE, 'fetch_zones', [_API]}, {'error', 'not_found'}) ->
    [] =:= pqc_kazoo_model:dedicated_zones(Model);
postcondition(Model, {'call', ?MODULE, 'fetch_hosts', [_API]}, {'ok', Hosts}) ->
    lists:usort(Hosts) =:= lists:usort(pqc_kazoo_model:dedicated_hosts(Model));
postcondition(Model, {'call', ?MODULE, 'fetch_hosts', [_API]}, {'error', 'not_found'}) ->
    [] =:= pqc_kazoo_model:dedicated_hosts(Model);

postcondition(_Model, {'call', ?MODULE, 'fetch_assigned', [_API, 'undefined']}, {'error', 'not_found'}) ->
    'true';
postcondition(Model, {'call', ?MODULE, 'fetch_assigned', [_API, AccountId]}, {'ok', []}) ->
    [] =:= pqc_kazoo_model:account_ips(Model, AccountId);
postcondition(Model, {'call', ?MODULE, 'fetch_assigned', [_API, AccountId]}, {'ok', ListedIPs}) ->
    lists:all(fun({IP, IPInfo}) ->
                      is_ip_listed(IP, IPInfo, ListedIPs)
              end
             ,pqc_kazoo_model:account_ips(Model, AccountId)
             );
postcondition(Model, {'call', ?MODULE, 'fetch_assigned', [_API, AccountId]}, {'error', 'not_found'}) ->
    [] =:= pqc_kazoo_model:account_ips(Model, AccountId);
postcondition(Model, {'call', ?MODULE, 'create_ip', [_API, ?DEDICATED(IP, _, _)]}, {'ok', _CreatedIP}) ->
    'undefined' =:= pqc_kazoo_model:dedicated_ip(Model, IP);
postcondition(Model, {'call', ?MODULE, 'create_ip', [_API, ?DEDICATED(IP, _, _)]}, {'error', 'conflict'}) ->
    'undefined' =/= pqc_kazoo_model:dedicated_ip(Model, IP);
postcondition(Model, {'call', ?MODULE, 'delete_ip', [_API, ?DEDICATED(IP, _, _)]}, {'ok', _Deleted}) ->
    'undefined' =/= pqc_kazoo_model:dedicated_ip(Model, IP);
postcondition(Model, {'call', ?MODULE, 'delete_ip', [_API, ?DEDICATED(IP, _, _)]}, {'error', 'not_found'}) ->
    'undefined' =:= pqc_kazoo_model:dedicated_ip(Model, IP).

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   timer:sleep(1000),
                   try run_commands(?MODULE, Cmds) of
                       {History, Model, Result} ->
                           cleanup(pqc_kazoo_model:api(Model)),
                           ?WHENFAIL(io:format("Final Model:~n~p~n~nFailing Cmds:~n~p~n"
                                              ,[pqc_kazoo_model:pp(Model), zip(Cmds, History)]
                                              )
                                    ,aggregate(command_names(Cmds), Result =:= 'ok')
                                    )
                   catch
                       _E:_R ->
                           ST = erlang:get_stacktrace(),
                           io:format("exception running commands: ~s:~p~n", [_E, _R]),
                           [io:format("~p~n", [S]) || S <- ST],
                           cleanup(),
                           'false'
                   end

               end
              )
           ).

-spec correct_parallel() -> any().
correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                   cleanup(),

                   ?WHENFAIL(io:format("S: ~p~nP: ~p~n", [Sequential, Parallel])
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

%%% Helpers
-spec do_dedicated_ips_exist(pqc_kazoo_model:model(), [dedicated()]) ->
                                    boolean().
do_dedicated_ips_exist(Model, Dedicateds) ->
    lists:all(fun(?DEDICATED(IP, _, _)) -> pqc_kazoo_model:does_ip_exist(Model, IP) end
             ,Dedicateds
             ).

-spec are_dedicated_ips_unassigned(pqc_kazoo_model:model(), [dedicated()]) ->
                                          boolean().
are_dedicated_ips_unassigned(Model, Dedicateds) ->
    lists:all(fun(?DEDICATED(IP, _, _)) -> pqc_kazoo_model:is_ip_unassigned(Model, IP) end
             ,Dedicateds
             ).

-spec assign_dedicated_ips(pqc_kazoo_model:model(), pqc_cb_accounts:account_id(), [dedicated()]) ->
                                  pqc_kazoo_model:model().
assign_dedicated_ips(Model, AccountId, Dedicateds) ->
    lists:foldl(fun(?DEDICATED(IP, _, _), Mdl) ->
                        pqc_kazoo_model:assign_dedicated_ip(Mdl, AccountId, IP)
                end
               ,Model
               ,Dedicateds
               ).

-spec is_ip_listed(ne_binary(), pqc_kazoo_model:dedicated_ip(), kz_json:objects()) ->
                          boolean().
is_ip_listed(IP, IPInfo, ListedIPs) ->
    Host = maps:get('host', IPInfo, 'undefined'),
    Zone = maps:get('zone', IPInfo, 'undefined'),

    lists:any(fun(ListedIP) ->
                      IP =:= kz_json:get_ne_binary_value(<<"ip">>, ListedIP)
                          andalso Host =:= kz_json:get_ne_binary_value(<<"host">>, ListedIP)
                          andalso Zone =:= kz_json:get_ne_binary_value(<<"zone">>, ListedIP)
              end
             ,ListedIPs
             ).

-spec all_requested_are_listed(ne_binary(), [dedicated()], kz_json:objects()) -> boolean().
all_requested_are_listed(AccountId, Dedicateds, ListedIPs) ->
    [] =:= lists:foldl(fun(ListedIP, Ds) ->
                               IP = kz_json:get_ne_binary_value(<<"ip">>, ListedIP),

                               case lists:keytake(IP, #dedicated.ip, Dedicateds) of
                                   'false' -> Ds;
                                   {'value', D, Ds1} ->
                                       case is_assigned(AccountId, D, ListedIP) of
                                           'true' -> Ds1;
                                           'false' -> Ds
                                       end
                               end
                       end
                      ,Dedicateds
                      ,ListedIPs
                      ).

-spec is_assigned(ne_binary(), dedicated(), kz_json:object()) -> boolean().
is_assigned(AccountId, ?DEDICATED(DIP, DHost, DZone), ListedIP) ->
    IP = kz_json:get_ne_binary_value(<<"ip">>, ListedIP),
    Host = kz_json:get_ne_binary_value(<<"host">>, ListedIP),
    Zone = kz_json:get_ne_binary_value(<<"zone">>, ListedIP),
    AssignedTo = kz_json:get_ne_binary_value(<<"assigned_to">>, ListedIP),
    Status = kz_json:get_ne_binary_value(<<"status">>, ListedIP),

    AccountId =:= AssignedTo
        andalso <<"assigned">> =:= Status
        andalso IP =:= DIP
        andalso Host =:= DHost
        andalso Zone =:= DZone.
