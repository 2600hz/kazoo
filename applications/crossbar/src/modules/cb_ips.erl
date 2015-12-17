%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_ips).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,post/1 ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ips">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ips">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ips">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.ips">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.ips">>, ?MODULE, 'delete').

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
    [?HTTP_GET, ?HTTP_POST].

allowed_methods(_) ->
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
    validate_ips(Context, cb_context:req_verb(Context)).

validate(Context, PathToken) ->
    validate_ips(Context, PathToken, cb_context:req_verb(Context)).

-spec validate_ips(cb_context:context(), ne_binary()) -> cb_context:context().
-spec validate_ips(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
validate_ips(Context, ?HTTP_GET) ->
    load_available(Context);
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
    maybe_assign_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_DELETE) ->
    release_ip(Context, IP).


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
    Ips = wh_json:get_value(<<"ips">>, ReqData, []),
    Props = [{<<"type">>, <<"ips">>}
             ,{<<"dedicated">>, erlang:length(Ips)}
            ],
    crossbar_services:maybe_dry_run(Context, Callback, Props).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Ip) ->
    Callback =
        fun() ->
                case cb_context:resp_status(Context) of
                    'success' -> assign_ip(Context, Ip);
                    _ -> Context
                end
        end,
    crossbar_services:maybe_dry_run(Context, Callback, <<"ips">>).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _DocId) -> Context.

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
    Zone = wh_json:get_value(<<"zone">>, QS),
    case kz_ips:available(Zone) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,JObjs
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
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
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,JObjs
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
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
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,Zones
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
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
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,Hosts
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
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
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,clean_ip(IPJSON)
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_assign_ips(cb_context:context()) -> cb_context:context().
-spec maybe_assign_ips(cb_context:context(), ne_binaries()) -> cb_context:context().
maybe_assign_ips(Context) ->
    ReqData = cb_context:req_data(Context),
    maybe_assign_ips(
      cb_context:set_resp_status(Context, 'success')
      ,wh_json:get_value(<<"ips">>, ReqData, [])
     ).

maybe_assign_ips(Context, []) -> Context;
maybe_assign_ips(Context, [Ip|Ips]) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Context1 = maybe_assign_ip(Context, Ip),
            maybe_assign_ips(Context1, Ips);
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_assign_ip(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_assign_ip(Context, Ip) ->
    case kz_ip:is_available(Ip) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_validation_error(
              <<"ip">>
              ,<<"forbidden">>
              ,wh_json:from_list(
                 [{<<"cause">>, Ip}
                  ,{<<"message">>, <<"ip already assigned">>}
                 ])
              ,Context
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_ips(cb_context:context()) -> cb_context:context().
-spec assign_ips(cb_context:context(), ne_binaries(), wh_json:objects()) -> cb_context:context().
assign_ips(Context) ->
    ReqData = cb_context:req_data(Context),
    assign_ips(Context, wh_json:get_value(<<"ips">>, ReqData, []), []).

assign_ips(Context, [], RespData) ->
    cb_context:set_resp_data(
      cb_context:set_resp_status(Context, 'success')
      ,RespData
     );
assign_ips(Context, [Ip|Ips], RespData) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assign(AccountId, Ip) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            assign_ips(Context, Ips, [clean_ip(IPJSON)|RespData]);
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_ip(cb_context:context(), ne_binary()) -> cb_context:context().
assign_ip(Context, Ip) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assign(AccountId, Ip) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,clean_ip(IPJSON)
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec release_ip(cb_context:context(), ne_binary()) -> cb_context:context().
release_ip(Context, Id) ->
    case kz_ip:release(Id) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,clean_ip(IPJSON)
             );
        {'error', Reason} ->
            cb_context:add_system_error(
              'datastore_fault'
              ,wh_json:from_list([{<<"cause">>, Reason}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_ip(wh_json:object()) -> wh_json:object().
clean_ip(JObj) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"id">>, wh_doc:id(JObj)}
         ,{<<"ip">>, wh_doc:id(JObj)}
         ,{<<"zone">>, wh_json:get_value(<<"pvt_zone">>, JObj)}
         ,{<<"host">>, wh_json:get_value(<<"pvt_host">>, JObj)}
         ,{<<"status">>, wh_json:get_value(<<"pvt_status">>, JObj)}
         ,{<<"type">>, wh_doc:type(JObj)}
         ,{<<"assigned_to">>, wh_json:get_value(<<"pvt_assigned_to">>, JObj)}
        ])
     ).
