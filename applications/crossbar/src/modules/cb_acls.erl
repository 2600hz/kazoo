%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% ACLs from 7 to 77
%%%
%%% @end
%%% Contributors: Karl Anderson
%%%               James Aimonetti
%%%               Edouard Swiac
%%%-------------------------------------------------------------------
-module(cb_acls).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
        ]).

-include("../crossbar.hrl").
-define(ECALLMGR, <<"ecallmgr">>).
-define(ECALLMGR_ACLS, <<"acls">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.acls">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.acls">>, ?MODULE, 'resource_exists'),
    crossbar_bindings:bind(<<"*.validate.acls">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /acls => []
%%    /acls/foo => [<<"foo">>]
%%    /acls/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /acls mights load a list of skel objects
%% /acls/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Req = [{<<"Category">>, ?ECALLMGR}
           ,{<<"Key">>, ?ECALLMGR_ACLS}
           ,{<<"Default">>, wh_json:new()}
           ,{<<"Node">>, wh_util:to_binary(node())}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("looking up acls from sysconf", []),
    ReqResp = whapps_util:amqp_pool_request(Req
                                            ,fun wapi_sysconf:publish_get_req/1
                                            ,fun wapi_sysconf:get_resp_v/1
                                            ,2000
                                           ),
    case ReqResp of
        {'error', _R} ->
            lager:debug("unable to get acls from sysconf: ~p", [_R]),
            cb_context:add_system_error('datastore_fault', Context);
        {'ok', JObj} ->
            ACLs = wh_json:get_value(<<"Value">>, JObj, wh_json:new()),
            Context#cb_context{resp_data=ACLs, resp_status='success'}
    end.
