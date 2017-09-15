%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_ips).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,post/1 ,post/2
        ,put/1
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(ASSIGNED, <<"assigned">>).
-define(ZONES, <<"zones">>).
-define(HOSTS, <<"hosts">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
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
    cb_context:put_reqid(Context),
    authorize(Context, cb_context:req_nouns(Context)).

authorize(Context, [{<<"ips">>, []}]) ->
    cb_context:is_superduper_admin(Context);
authorize(_Context, _Nouns) ->
    'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST].

allowed_methods(?ASSIGNED) ->
    [?HTTP_GET];
allowed_methods(?ZONES) ->
    [?HTTP_GET];
allowed_methods(?HOSTS) ->
    [?HTTP_GET];
allowed_methods(_IPAddress) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /dedicated_ips => []
%%    /dedicated_ips/foo => [<<"foo">>]
%%    /dedicated_ips/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /dedicated_ips mights load a list of skel objects
%% /dedicated_ips/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    cb_context:put_reqid(Context),
    validate_ips(Context, cb_context:req_verb(Context)).

validate(Context, PathToken) ->
    cb_context:put_reqid(Context),
    validate_ips(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_ips(cb_context:context(), ne_binary()) -> cb_context:context().
-spec validate_ips(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
validate_ips(Context, ?HTTP_GET) ->
    load_available(Context);
validate_ips(Context, ?HTTP_PUT) ->
    maybe_create_ip(Context);
validate_ips(Context, ?HTTP_POST) ->
    maybe_assign_ips(Context).

validate_ips(Context, ?ASSIGNED, ?HTTP_GET) ->
    load_assigned(Context);
validate_ips(Context, ?ZONES, ?HTTP_GET) ->
    load_zones(Context);
validate_ips(Context, ?HOSTS, ?HTTP_GET) ->
    load_hosts(Context);
validate_ips(Context, IP, ?HTTP_GET) ->
    load_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_POST) ->
    validate_ip_not_in_use(Context, IP);
validate_ips(Context, IP, ?HTTP_DELETE) ->
    case kz_ip:fetch(IP) of
        {'ok', _} -> cb_context:set_resp_status(Context, 'success');
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, IP, Context)
    end.

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    Callback =
        fun() ->
                case cb_context:resp_status(Context) of
                    'success' -> assign_ips(Context);
                    _ -> Context
                end
        end,
    ReqData = cb_context:req_data(Context),
    IPs = kz_json:get_value(<<"ips">>, ReqData, []),
    Props = [{<<"type">>, <<"ips">>}
            ,{<<"dedicated">>, erlang:length(IPs)}
            ],
    crossbar_services:maybe_dry_run(Context, Callback, Props).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, IP) ->
    Callback =
        fun() ->
                case cb_context:resp_status(Context) of
                    'success' -> assign_ip(Context, IP);
                    _ -> Context
                end
        end,
    crossbar_services:maybe_dry_run(Context, Callback, <<"ips">>).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, IP) ->
    release_or_delete_ip(Context, IP, cb_context:req_nouns(Context)).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_available(cb_context:context()) -> cb_context:context().
load_available(Context) ->
    QS = cb_context:query_string(Context),
    Zone = kz_json:get_value(<<"zone">>, QS),
    case kz_ips:available(Zone) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,JObjs
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_assigned(cb_context:context()) -> cb_context:context().
load_assigned(Context) ->
    AccountId = cb_context:account_id(Context),
    case kz_ips:assigned(AccountId) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,JObjs
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_zones(cb_context:context()) -> cb_context:context().
load_zones(Context) ->
    case kz_ips:zones() of
        {'ok', Zones} ->
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,Zones
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_ip(cb_context:context(), ne_binary()) -> cb_context:context().
load_ip(Context, Id) ->
    case kz_ip:fetch(Id) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,clean_ip(IPJSON)
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

-define(IP_SCHEMA, kz_json:from_list([{<<"type">>, <<"string">>}])).
-define(HOST_SCHEMA, kz_json:from_list([{<<"type">>, <<"string">>}])).
-define(ZONE_SCHEMA, kz_json:from_list([{<<"type">>, <<"string">>}])).

-define(CREATE_IP_SCHEMA
       ,kz_json:from_list([{<<"ip">>, ?IP_SCHEMA}
                          ,{<<"host">>, ?HOST_SCHEMA}
                          ,{<<"zone">>, ?ZONE_SCHEMA}
                          ])
       ).

-spec maybe_create_ip(cb_context:context()) -> cb_context:context().
maybe_create_ip(Context) ->
    cb_context:validate_request_data(?CREATE_IP_SCHEMA, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_assign_ips(cb_context:context()) -> cb_context:context().
maybe_assign_ips(Context) ->
    OnSuccess = fun validate_ips_not_in_use/1,
    cb_context:validate_request_data(<<"ips">>, Context, OnSuccess).

-spec validate_ips_not_in_use(cb_context:context()) -> cb_context:context().
-spec validate_ips_not_in_use(cb_context:context(), ne_binaries()) -> cb_context:context().
validate_ips_not_in_use(Context) ->
    validate_ips_not_in_use(Context, cb_context:req_value(Context, <<"ips">>)).

validate_ips_not_in_use(Context, IPs) ->
    lists:foldl(fun validate_ip_not_in_use/2, Context, IPs).

-spec validate_ip_not_in_use(ne_binary() | cb_context:context(), ne_binary() | cb_context:context()) ->
                                    cb_context:context().
validate_ip_not_in_use(<<_/binary>> = IP, Context) ->
    validate_ip_not_in_use(Context, IP);
validate_ip_not_in_use(Context, <<_/binary>> = IP) ->
    validate_ip_not_in_use(Context, IP, cb_context:resp_status(Context)).

-spec validate_ip_not_in_use(cb_context:context(), ne_binary(), crossbar_status()) ->
                                    cb_context:context().
validate_ip_not_in_use(Context, IP, 'error') ->
    case kz_ip:is_available(IP) of
        'true' -> Context;
        'false' -> error_ip_assigned(Context, IP);
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end;
validate_ip_not_in_use(Context, IP, _Status) ->
    case kz_ip:is_available(IP) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' -> error_ip_assigned(Context, IP);
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

-spec error_ip_assigned(cb_context:context(), ne_binary()) -> cb_context:context().
error_ip_assigned(Context, IP) ->
    Msg = kz_json:from_list([{<<"cause">>, IP}
                            ,{<<"message">>, <<"ip already assigned">>}
                            ]),
    cb_context:add_validation_error(<<"ip">>, <<"forbidden">>, Msg, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_ips(cb_context:context()) -> cb_context:context().
-spec assign_ips(cb_context:context(), ne_binaries(), kz_json:objects()) -> cb_context:context().
assign_ips(Context) ->
    ReqData = cb_context:req_data(Context),
    assign_ips(Context, kz_json:get_value(<<"ips">>, ReqData, []), []).

assign_ips(Context, [], RespData) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                            ,RespData
                            );
assign_ips(Context, [IP|IPs], RespData) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assign(AccountId, IP) of
        {'ok', AssignedIP} ->
            IPJSON = kz_ip:to_json(AssignedIP),
            assign_ips(Context, IPs, [clean_ip(IPJSON)|RespData]);
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_ip(cb_context:context(), ne_binary()) -> cb_context:context().
assign_ip(Context, IP) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assign(AccountId, IP) of
        {'ok', AssignedIP} ->
            IPJSON = kz_ip:to_json(AssignedIP),
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,clean_ip(IPJSON)
                                    );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault'
                                       ,kz_json:from_list([{<<"cause">>, Reason}])
                                       ,Context
                                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec release_or_delete_ip(cb_context:context(), ne_binary(), req_nouns()) -> cb_context:context().
release_or_delete_ip(Context, IP, [{<<"ips">>, [IP]}]) ->
    delete_ip(Context, IP);
release_or_delete_ip(Context, IP, [{<<"ips">>, [IP]}, {<<"accounts">>, [_]}]) ->
    release_ip(Context, IP).

-spec release_ip(cb_context:context(), ne_binary()) -> cb_context:context().
release_ip(Context, Id) ->
    case kz_ip:release(Id) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                                    ,clean_ip(IPJSON)
                                    );
        {'error', Reason} ->
            crossbar_doc:handle_datamgr_errors(Reason, Id, Context)
    end.

-spec delete_ip(cb_context:context(), ne_binary()) -> cb_context:context().
delete_ip(Context, IP) ->
    case kz_ip:delete(IP) of
        {'ok', Deleted} -> crossbar_doc:handle_json_success(Context, Deleted);
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, IP, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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
