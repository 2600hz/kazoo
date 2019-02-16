%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_ips).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,post/1 ,post/2
        ,put/1
        ,delete/1, delete/2
        ]).

-include("crossbar.hrl").

-define(ASSIGNED, <<"assigned">>).
-define(ZONES, <<"zones">>).
-define(HOSTS, <<"hosts">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ips">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ips">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ips">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.ips">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ips">>, ?MODULE, 'put'),
    crossbar_bindings:bind(<<"*.execute.delete.ips">>, ?MODULE, 'delete').

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    _ = cb_context:put_reqid(Context),
    authorize(Context, cb_context:req_nouns(Context)).

authorize(Context, [{<<"ips">>, _}]) ->
    cb_context:is_superduper_admin(Context);
authorize(_Context, _Nouns) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ASSIGNED) ->
    [?HTTP_GET];
allowed_methods(?ZONES) ->
    [?HTTP_GET];
allowed_methods(?HOSTS) ->
    [?HTTP_GET];
allowed_methods(_IPAddress) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /dedicated_ips => []
%%    /dedicated_ips/foo => [<<"foo">>]
%%    /dedicated_ips/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /dedicated_ips might load a list of skel objects
%% /dedicated_ips/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    _ = cb_context:put_reqid(Context),
    validate_ips(Context, cb_context:req_verb(Context)).

-spec validate_ips(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_ips(Context, ?HTTP_GET) ->
    load_available(Context);
validate_ips(Context, ?HTTP_PUT) ->
    maybe_create_ip(Context);
validate_ips(Context, ?HTTP_POST) ->
    validate_assign_ips(Context);
validate_ips(Context, ?HTTP_DELETE) ->
    validate_release_ips(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    _ = cb_context:put_reqid(Context),
    validate_ips(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_ips(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
validate_ips(Context, ?ASSIGNED, ?HTTP_GET) ->
    load_assigned(Context);
validate_ips(Context, ?ZONES, ?HTTP_GET) ->
    load_zones(Context);
validate_ips(Context, ?HOSTS, ?HTTP_GET) ->
    load_hosts(Context);
validate_ips(Context, IP, ?HTTP_GET) ->
    load_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_POST) ->
    validate_assign_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_DELETE) ->
    validate_release_ip(Context, IP).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    assign_ips(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _IP) ->
    assign_ips(Context).

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    release_ips(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, IP) ->
    delete(Context, IP, cb_context:req_nouns(Context)).

-spec delete(cb_context:context(), path_token(), req_nouns()) -> cb_context:context().
delete(Context, IP, [{<<"ips">>, [IP]}]) ->
    delete_ip(Context, IP);
delete(Context, IP, [{<<"ips">>, [IP]}, {<<"accounts">>, [_]}]) ->
    release_ips(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    ReqData = cb_context:req_data(Context),
    IP = kz_json:get_ne_binary_value(<<"ip">>, ReqData),
    Zone = kz_json:get_ne_binary_value(<<"zone">>, ReqData),
    Host = kz_json:get_ne_binary_value(<<"host">>, ReqData),

    case kz_ip:create(IP, Zone, Host) of
        {'ok', IPJObj} ->
            JObj = kz_doc:leak_private_fields(IPJObj),
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, JObj}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, JObj}
                               ,{fun cb_context:set_resp_etag/2, crossbar_doc:rev_to_etag(JObj)}
                               ]);
        {'error', Error} ->
            lager:debug("failed to create ip ~s: ~p", [IP, Error]),
            crossbar_doc:handle_datamgr_errors(Error, IP, Context)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_available(cb_context:context()) -> cb_context:context().
load_available(Context) ->
    QS = cb_context:query_string(Context),
    Zone = kz_json:get_ne_binary_value(<<"zone">>, QS),
    case kz_ips:available(Zone) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,JObjs
                                    );
        {'error', 'not_found'} ->
            cb_context:add_system_error('not_found'
                                       ,kz_json:from_list([{<<"cause">>, Zone}])
                                       ,Context
                                       );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_assigned(cb_context:context()) -> cb_context:context().
load_assigned(Context) ->
    AccountId = cb_context:account_id(Context),
    case kz_ips:assigned(AccountId) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,JObjs
                                    );
        {'error', 'not_found'} ->
            cb_context:add_system_error('not_found'
                                       ,kz_json:from_list([{<<"cause">>, AccountId}])
                                       ,Context
                                       );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_zones(cb_context:context()) -> cb_context:context().
load_zones(Context) ->
    case kz_ips:zones() of
        {'ok', Zones} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,Zones
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('not_found'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_hosts(cb_context:context()) -> cb_context:context().
load_hosts(Context) ->
    case kz_ips:hosts() of
        {'ok', Hosts} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,Hosts
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_ip(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_ip(Context, Id) ->
    case kz_ip:fetch(Id) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,clean_ip(IPJSON)
                                    );
        {'error', 'not_found'} ->
            cb_context:add_system_error('not_found'
                                       ,kz_json:from_list([{<<"cause">>, Id}])
                                       ,Context
                                       );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_create_ip(cb_context:context()) -> cb_context:context().
maybe_create_ip(Context) ->
    maybe_create_ip(Context, cb_context:req_nouns(Context)).
-spec maybe_create_ip(cb_context:context(), req_nouns()) -> cb_context:context().
maybe_create_ip(Context, [{<<"ips">>, _}]) ->
    cb_context:validate_request_data(<<"ip">>, Context);
maybe_create_ip(Context, _Nouns) ->
    cb_context:add_system_error('forbidden', Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_assign_ips(cb_context:context()) -> cb_context:context().
validate_assign_ips(Context) ->
    Resp = kz_json:from_list(
             [{<<"success">>, kz_json:new()}
             ,{<<"error">>, kz_json:new()}
             ]
            ),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Resp}
              ],
    Context1 = cb_context:setters(Context, Setters),
    cb_context:validate_request_data(<<"ips">>, Context1, fun additional_assignment_validations/1).

-spec validate_assign_ip(cb_context:context(), path_token()) -> cb_context:context().
validate_assign_ip(Context, IP) ->
    ReqData = kz_json:from_list([{<<"ips">>, [IP]}]),
    validate_assign_ips(cb_context:set_req_data(Context, ReqData)).

-spec additional_assignment_validations(cb_context:context()) -> cb_context:context().
additional_assignment_validations(Context) ->
    IPs = [{IP, kz_ip:fetch(IP)}
           || IP <- cb_context:req_value(Context, <<"ips">>, [])
          ],
    additional_assignment_validations(Context, IPs, [], 0).

-spec additional_assignment_validations(cb_context:context(), kz_term:proplist(), kz_term:objects(), non_neg_integer()) ->
                                               cb_context:context().
additional_assignment_validations(Context, [], Assign, _Index) ->
    case cb_context:resp_status(Context) =:= 'success' of
        'false' -> Context;
        'true' ->
            maybe_dry_run_assignment(
              cb_context:store(Context, 'assign_ips', Assign)
             )
    end;
additional_assignment_validations(Context, [{Address, {'ok', JObj}}=IP|IPs], Assign, Index) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assigned_to(JObj) of
        'undefined' ->
            additional_assignment_validations(Context, IPs, [JObj|Assign], Index + 1);
        AccountId ->
            Context1 = validate_error_already_assigned(Context, Address, Index),
            additional_assignment_validations(Context1, IPs, Assign, Index + 1);
        _Else ->
            Context1 = validate_error_assigned(Context, IP, Index),
            additional_assignment_validations(Context1, IPs, Assign, Index + 1)
    end;
additional_assignment_validations(Context, [{Address, {'error', 'not_found'}}|IPs], Assign, Index) ->
    Context1 = validate_error_not_found(Context, Address, Index),
    additional_assignment_validations(Context1, IPs, Assign, Index + 1);
additional_assignment_validations(Context, [{_Address, {'error', Reason}}|_IPs], _Assign, _Index) ->
    cb_context:add_system_error('datastore_fault'
                               ,kz_json:from_list([{<<"cause">>, Reason}])
                               ,Context
                               ).

-spec validate_error_already_assigned(cb_context:context(), kz_term:ne_binary(), non_neg_integer()) ->
                                             cb_context:context().
validate_error_already_assigned(Context, IP, Index) ->
    Key = <<"ips.", (kz_term:to_binary(Index))/binary>>,
    Message = kz_json:from_list(
                [{<<"message">>, <<"ip already assigned">>}
                ,{<<"value">>, IP}
                ]),
    cb_context:add_validation_error(Key, <<"superfluous">>, Message, Context).

-spec validate_error_assigned(cb_context:context(), kz_term:ne_binary(), non_neg_integer()) ->
                                     cb_context:context().
validate_error_assigned(Context, IP, Index) ->
    Key = <<"ips.", (kz_term:to_binary(Index))/binary>>,
    Message = kz_json:from_list(
                [{<<"message">>, <<"ip assigned to another account">>}
                ,{<<"value">>, IP}
                ]),
    cb_context:add_validation_error(Key, <<"forbidden">>, Message, Context).

-spec validate_error_not_found(cb_context:context(), kz_term:ne_binary(), non_neg_integer()) ->
                                      cb_context:context().
validate_error_not_found(Context, IP, Index) ->
    Key = <<"ips.", (kz_term:to_binary(Index))/binary>>,
    Message = kz_json:from_list(
                [{<<"message">>, <<"ip not found">>}
                ,{<<"value">>, IP}
                ]),
    cb_context:add_validation_error(Key, <<"not_found">>, Message, Context).

-spec maybe_dry_run_assignment(cb_context:context()) -> cb_context:context().
maybe_dry_run_assignment(Context) ->
    ProposedJObjs = cb_context:fetch(Context, 'assign_ips', []),
    crossbar_services:maybe_dry_run(Context, [], ProposedJObjs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign_ips(cb_context:context()) -> cb_context:context().
assign_ips(Context) ->
    JObjs = cb_context:fetch(Context, 'assign_ips', []),
    assign_ips(Context, JObjs).

-spec assign_ips(cb_context:context(), kz_json:objects()) -> cb_context:context().
assign_ips(Context, []) ->
    _ = crossbar_services:reconcile(cb_context:account_id(Context)),
    Context;
assign_ips(Context, [JObj|JObjs]) ->
    Context1 =
        case kz_ip:assign(cb_context:account_id(Context), JObj) of
            {'ok', AssignedJObj} ->
                Resp = kz_json:set_value([<<"success">>, kz_doc:id(JObj)]
                                        ,clean_ip(kz_ip:to_json(AssignedJObj))
                                        ,cb_context:resp_data(Context)
                                        ),
                cb_context:set_resp_data(Context, Resp);
            {'error', Reason} ->
                Message = kz_json:from_list(
                            [{<<"message">>, Reason}]
                           ),
                Resp = kz_json:set_value([<<"error">>, kz_doc:id(JObj)]
                                        ,Message
                                        ,cb_context:resp_data(Context)
                                        ),
                cb_context:set_resp_data(Context, Resp)
        end,
    assign_ips(Context1, JObjs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_release_ips(cb_context:context()) -> cb_context:context().
validate_release_ips(Context) ->
    Resp = kz_json:from_list(
             [{<<"success">>, kz_json:new()}
             ,{<<"error">>, kz_json:new()}
             ]
            ),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Resp}
              ],
    Context1 = cb_context:setters(Context, Setters),
    cb_context:validate_request_data(<<"ips">>, Context1, fun additional_release_validations/1).

-spec validate_release_ip(cb_context:context(), path_token()) -> cb_context:context().
validate_release_ip(Context, IP) ->
    ReqData = kz_json:from_list([{<<"ips">>, [IP]}]),
    validate_release_ips(cb_context:set_req_data(Context, ReqData)).

-spec additional_release_validations(cb_context:context()) -> cb_context:context().
additional_release_validations(Context) ->
    IPs = [{IP, kz_ip:fetch(IP)}
           || IP <- cb_context:req_value(Context, <<"ips">>, [])
          ],
    additional_release_validations(Context, IPs, [], 0).

-spec additional_release_validations(cb_context:context(), kz_term:proplist(), kz_json:objects(), non_neg_integer()) ->
                                            cb_context:context().
additional_release_validations(Context, [], Release, _Index) ->
    cb_context:store(Context, 'release_ips', Release);
additional_release_validations(Context, [{Address, {'ok', JObj}}=IP|IPs], Release, Index) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assigned_to(JObj) of
        'undefined' ->
            Context1 = validate_error_not_assigned(Context, Address, Index),
            additional_release_validations(Context1, IPs, Release, Index + 1);
        AccountId ->
            additional_release_validations(Context, IPs, [JObj|Release], Index + 1);
        _Else ->
            Context1 = validate_error_assigned(Context, IP, Index),
            additional_assignment_validations(Context1, IPs, Release, Index + 1)
    end;
additional_release_validations(Context, [{Address, {'error', 'not_found'}}|IPs], Release, Index) ->
    Context1 = validate_error_not_found(Context, Address, Index),
    additional_release_validations(Context1, IPs, Release, Index + 1);
additional_release_validations(Context, [{_Address, {'error', Reason}}|_IPs], _Release, _Index) ->
    cb_context:add_system_error('datastore_fault'
                               ,kz_json:from_list([{<<"cause">>, Reason}])
                               ,Context
                               ).

-spec validate_error_not_assigned(cb_context:context(), kz_term:ne_binary(), non_neg_integer()) ->
                                         cb_context:context().
validate_error_not_assigned(Context, IP, Index) ->
    Key = <<"ips.", (kz_term:to_binary(Index))/binary>>,
    Message = kz_json:from_list(
                [{<<"message">>, <<"ip not assigned">>}
                ,{<<"value">>, IP}
                ]),
    cb_context:add_validation_error(Key, <<"superfluous">>, Message, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release_ips(cb_context:context()) -> cb_context:context().
release_ips(Context) ->
    JObjs = cb_context:fetch(Context, 'release_ips', []),
    release_ips(Context, JObjs).

-spec release_ips(cb_context:context(), kz_json:objects()) -> cb_context:context().
release_ips(Context, []) ->
    _ = crossbar_services:reconcile(cb_context:account_id(Context)),
    Context;
release_ips(Context, [JObj|JObjs]) ->
    Context1 =
        case kz_ip:release(kz_doc:id(JObj)) of
            {'ok', ReleasedJObj} ->
                Resp = kz_json:set_value([<<"success">>, kz_doc:id(JObj)]
                                        ,clean_ip(kz_ip:to_json(ReleasedJObj))
                                        ,cb_context:resp_data(Context)
                                        ),
                cb_context:set_resp_data(Context, Resp);
            {'error', Reason} ->
                Message = kz_json:from_list(
                            [{<<"message">>, Reason}]
                           ),
                Resp = kz_json:set_value([<<"error">>, kz_doc:id(JObj)]
                                        ,Message
                                        ,cb_context:resp_data(Context)
                                        ),
                cb_context:set_resp_data(Context, Resp)
        end,
    release_ips(Context1, JObjs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_ip(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
delete_ip(Context, IP) ->
    %% _ = crossbar_services:reconcile(cb_context:account_id(Context)),
    case kz_ip:delete(IP) of
        {'ok', Deleted} -> crossbar_doc:handle_json_success(Deleted, Context);
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, IP, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec clean_ip(kz_json:object()) -> kz_json:object().
clean_ip(JObj) ->
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(JObj)}
      ,{<<"ip">>, kz_doc:id(JObj)}
      ,{<<"zone">>, kz_json:get_value(<<"pvt_zone">>, JObj)}
      ,{<<"host">>, kz_json:get_value(<<"pvt_host">>, JObj)}
      ,{<<"status">>, kz_json:get_value(<<"pvt_status">>, JObj)}
      ,{<<"type">>, kz_doc:type(JObj)}
      ,{<<"assigned_to">>, kz_json:get_value(<<"pvt_assigned_to">>, JObj)}
      ]).
