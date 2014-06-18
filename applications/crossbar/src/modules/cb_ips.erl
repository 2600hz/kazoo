%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ips).
-export([init/0
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/2
        ]).

-include("../crossbar.hrl").

-define(ASSIGNED, <<"assigned">>).

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
    crossbar_bindings:bind(<<"*.validate.ips">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ASSIGNED) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /dedicated_ips => []
%%    /dedicated_ips/foo => [<<"foo">>]
%%    /dedicated_ips/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?ASSIGNED) -> 'true'.

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
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Status) ->
    validate_dedicated_ips(Context, Status, cb_context:req_verb(Context)).

-spec validate_dedicated_ips(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_dedicated_ips(Context, ?ASSIGNED, ?HTTP_GET) ->
    case kz_ips:assigned(cb_context:account_id(Context)) of
        {'ok', JObjs} ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,JObjs
             );
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    end.
