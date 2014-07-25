%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
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
         ,post/2
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
    [?HTTP_GET].

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
    load_available(Context).

validate_ips(Context, ?ASSIGNED, ?HTTP_GET) ->
    load_assigned(Context);
validate_ips(Context, ?ZONES, ?HTTP_GET) ->
    load_zones(Context);
validate_ips(Context, ?HOSTS, ?HTTP_GET) ->
    load_hosts(Context);
validate_ips(Context, IP, ?HTTP_GET) ->
    load_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_POST) ->
    assign_ip(Context, IP);
validate_ips(Context, IP, ?HTTP_DELETE) ->
    release_ip(Context, IP).


-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _DocId) -> Context.

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
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
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
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_zones(cb_context:context()) -> cb_context:context().
load_zones(Context) ->
    case kz_ips:zones() of
        {'ok', JObjs} ->
            cb_context:set_resp_data(
                cb_context:set_resp_status(Context, 'success')
                ,JObjs
            );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_hosts(cb_context:context()) -> cb_context:context().
load_hosts(Context) ->
    case kz_ips:hosts() of
        {'ok', JObjs} ->
            cb_context:set_resp_data(
                cb_context:set_resp_status(Context, 'success')
                ,JObjs
            );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
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
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_ip(cb_context:context(), ne_binary()) -> cb_context:context().
assign_ip(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    case kz_ip:assign(AccountId, Id) of
        {'ok', IP} ->
            IPJSON = kz_ip:to_json(IP),
            cb_context:set_resp_data(
                cb_context:set_resp_status(Context, 'success')
                ,clean_ip(IPJSON)
            );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
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
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_ip(wh_json:object()) -> wh_json:object().
clean_ip(JObj) ->
    wh_json:from_list(
        props:filter_undefined([
            {<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
            ,{<<"ip">>, wh_json:get_value(<<"_id">>, JObj)}
            ,{<<"zone">>, wh_json:get_value(<<"pvt_zone">>, JObj)}
            ,{<<"host">>, wh_json:get_value(<<"pvt_host">>, JObj)}
            ,{<<"status">>, wh_json:get_value(<<"pvt_status">>, JObj)}
            ,{<<"type">>, wh_json:get_value(<<"pvt_type">>, JObj)}
            ,{<<"assigned_to">>, wh_json:get_value(<<"pvt_assigned_to">>, JObj)}
        ])
    ).
