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

-export([list_ips/2
        ,assign_ips/3
        ,remove_ip/3
        ,fetch_ip/3
        ,assign_ip/3
        ,fetch_hosts/2
        ,fetch_zones/2
        ,fetch_assigned/2
        ,create_ip/2

        ,ips_url/0, ips_url/1
        ]).

-export([cleanup/0, cleanup/1]).

-export_type([dedicated/0]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").

-define(DEDICATED(IP, Host, Zone), {'dedicated', IP, Host, Zone}).
-type dedicated() :: ?DEDICATED(ne_binary(), ne_binary(), ne_binary()).

-spec ips_url() -> string().
-spec ips_url(pqc_cb_accounts:account_id()) -> string().
-spec ip_url(pqc_cb_accounts:account_id(), ne_binary()) -> string().
ips_url() ->
    string:join([pqc_cb_api:v2_base_url(), "ips"], "/").

ips_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ips"], "/").

ip_url(AccountId, IP) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "ips", kz_term:to_list(IP)], "/").

-spec list_ips(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                      {'ok', kz_json:objects()} |
                      {'error', 'not_found'}.
list_ips(API, AccountId) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ips_url(AccountId)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("listing IPs errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec assign_ips(pqc_cb_api:state(), pqc_cb_accounts:account_id(), [dedicated()]) ->
                        {'ok', kz_json:objects()} |
                        {'error', 'not_found'}.
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
remove_ip(API, AccountId, ?DEDICATED(IP, _, _)) ->
    case pqc_cb_api:make_request([200]
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

-spec fetch_hosts(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                         {'ok', ne_binaries()} |
                         {'error', 'not_found'}.
fetch_hosts(API, AccountId) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url(AccountId, "hosts")
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("fetch hosts errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec fetch_zones(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                         {'ok', ne_binaries()} |
                         {'error', 'not_found'}.
fetch_zones(API, AccountId) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,ip_url(AccountId, "zones")
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
                       {'error', 'not_found'}.
create_ip(API, ?DEDICATED(IP, Host, Zone)) ->
    Data = kz_json:from_list([{<<"ip">>, IP}
                             ,{<<"host">>, Host}
                             ,{<<"zone">>, Zone}
                             ]),
    Envelope = pqc_cb_api:create_envelope(Data),
    case pqc_cb_api:make_request([200]
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
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.


-define(ACCOUNT_NAMES, [<<"account_for_ips">>]).

-spec cleanup() -> any().
-spec cleanup(pqc_cb_api:state()) -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    pqc_cb_service_plans:cleanup(),
    cleanup(pqc_cb_api:authenticate()).

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),

    pqc_cb_api:cleanup(API).

-spec command(any()) -> proper_types:type().
command(Model) ->
    API = pqc_kazoo_model:api(Model),

    AccountName = account_name(),
    AccountId = pqc_cb_accounts:symbolic_account_id(Model, AccountName),

    oneof([pqc_cb_accounts:command(Model, AccountName)
          ,{'call', ?MODULE, 'list_ips', [API, AccountId]}
          ,{'call', ?MODULE, 'assign_ips', [API, AccountId, ips()]}
          ,{'call', ?MODULE, 'remove_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'fetch_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'assign_ip', [API, AccountId, ip()]}
          ,{'call', ?MODULE, 'fetch_hosts', [API, AccountId]}
          ,{'call', ?MODULE, 'fetch_zones', [API, AccountId]}
          ,{'call', ?MODULE, 'fetch_assigned', [API, AccountId]}
          ,{'call', ?MODULE, 'create_ip', [API, ip()]}
          ]
         ).

account_name() ->
    oneof(?ACCOUNT_NAMES).

ip() ->
    oneof([?DEDICATED(<<"1.2.3.4">>, <<"a.host.com">>, <<"zone-1">>)]).

ips() ->
    non_empty(ip()).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model, APIResp, {'call', _, 'create_account', _Args}=Call) ->
    pqc_cb_accounts:next_state(Model, APIResp, Call).

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model, {'call', _, 'create_account', _Args}=Call, APIResult) ->
    pqc_cb_accounts:postcondition(Model, Call, APIResult).

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
