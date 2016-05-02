%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_channels).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/1
        ,validate/1, validate/2
        ,post/2
        ]).

-include("crossbar.hrl").

-type endpoints_return() :: {wh_json:objects(), cb_context:context()}.

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
    cb_modules_util:bind(?MODULE
                        ,[{<<"*.allowed_methods.channels">>, 'allowed_methods'}
                         ,{<<"*.resource_exists.channels">>, 'resource_exists'}
                         ,{<<"*.content_types_provided.channels">>, 'content_types_provided'}
                         ,{<<"*.validate.channels">>, 'validate'}
                         ,{<<"*.execute.post.channels">>, 'post'}
                         ]).

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
allowed_methods(_UUID) ->
    [?HTTP_GET, ?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /channels => []
%%    /channels/foo => [<<"foo">>]
%%    /channels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_UUID) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /channels mights load a list of channel objects
%% /channels/123 might load the channel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_channels(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_channel(Context, Id, cb_context:req_verb(Context)).

-spec validate_channels(cb_context:context(), http_method()) -> cb_context:context().
validate_channels(Context, ?HTTP_GET) ->
    summary(Context).

-spec validate_channel(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_channel(Context, Id, ?HTTP_GET) ->
    read(cb_context:set_resp_data(Context, wh_json:new()), Id);
validate_channel(Context, Id, ?HTTP_POST) ->
    update(Context, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _UUID) ->
    cb_context:set_resp_status(Context, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, CallId) ->
    case channels_query(CallId) of
        {'ok', []} ->
            lager:debug("no channel resp for ~s", [CallId]),
            crossbar_util:response_bad_identifier(CallId, Context);
        {'ok', StatusJObjs} ->
            case find_channel(cb_context:account_id(Context), CallId, StatusJObjs) of
                'undefined' ->
                    lager:warning("trying to get info about a channel ~s not in the account ~s", [CallId, cb_context:account_id(Context)]),
                    crossbar_util:response_bad_identifier(CallId, Context);
                Channel ->
                    crossbar_util:response(normalize_channel(Channel), Context)
            end;
        {'returned', JObj, _BR} ->
            lager:debug("return: ~p", [_BR]),
            crossbar_util:response(JObj, Context);
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            crossbar_util:response_datastore_timeout(Context)
    end.

-spec channels_query(ne_binary()) -> wh_amqp_worker:request_return().
channels_query(CallId) ->
    Req = [{<<"Call-ID">>, CallId}
          ,{<<"Fields">>, <<"all">>}
          ,{<<"Active-Only">>, 'true'}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    wh_amqp_worker:call_collect(Req
                               ,fun wapi_call:publish_query_channels_req/1
                               ,{'ecallmgr', fun wapi_call:query_channels_resp_v/1}
                               ).

-spec find_channel(ne_binary(), ne_binary(), wh_json:objects()) -> api_object().
find_channel(_AccountId, _CallId, []) -> 'undefined';
find_channel(AccountId, CallId, [StatusJObj|JObjs]) ->
    Channel = wh_json:get_value([<<"Channels">>, CallId], StatusJObj),
    case wh_json:get_value(<<"Account-ID">>, Channel) of
        AccountId -> Channel;
        _AccountId -> find_channel(AccountId, CallId, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, CallId) ->
    Context1 = read(Context, CallId),

    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' -> maybe_execute_command(Context1, CallId)
    end.

-spec maybe_execute_command(cb_context:context(), ne_binary()) -> cb_context:context().
-spec maybe_execute_command(cb_context:context(), ne_binary(), api_binary()) -> cb_context:context().
maybe_execute_command(Context, CallId) ->
    maybe_execute_command(Context, CallId, cb_context:req_value(Context, <<"action">>)).

maybe_execute_command(Context, Transferor, <<"transfer">>) ->
    maybe_transfer(Context, Transferor);
maybe_execute_command(Context, CallId, <<"hangup">>) ->
    maybe_hangup(Context, CallId);
maybe_execute_command(Context, CallId, <<"break">>) ->
    maybe_break(Context, CallId);
maybe_execute_command(Context, CallId, <<"callflow">>) ->
    maybe_callflow(Context, CallId);
maybe_execute_command(Context, _CallId, _Command) ->
    lager:debug("unknown command: ~s", [_Command]),
    crossbar_util:response_invalid_data(cb_context:doc(Context), Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"channels">>, []}, {<<"users">>, [UserId]} | _] ->
            user_summary(Context, UserId);
        [{<<"channels">>, []}, {<<"devices">>, [DeviceId]} | _] ->
            device_summary(Context, DeviceId);
        [{<<"channels">>, []}, {<<"groups">>, [GroupId]} | _] ->
            group_summary(Context, GroupId);
        [{<<"channels">>, []}, {<<"accounts">>, [_AccountId]} | _] ->
            lager:debug("getting account summary"),
            account_summary(Context);
        _Nouns ->
            lager:debug("unexpected nouns: ~p", [_Nouns]),
            crossbar_util:response_faulty_request(Context)
    end.

-spec device_summary(cb_context:context(), ne_binary()) -> cb_context:context().
device_summary(Context, DeviceId) ->
    get_channels(Context, [cb_context:doc(crossbar_doc:load(DeviceId, Context))], fun wapi_call:publish_query_user_channels_req/1).

-spec user_summary(cb_context:context(), ne_binary()) -> cb_context:context().
user_summary(Context, UserId) ->
    {UserEndpoints, Context1} = user_endpoints(Context, UserId),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' ->
            get_channels(Context
                         ,UserEndpoints
                         ,fun wapi_call:publish_query_user_channels_req/1
                        )
    end.

-spec user_endpoints(cb_context:context(), ne_binary()) ->
                            endpoints_return().
user_endpoints(Context, UserId) ->
    Options = [{'key', [UserId, <<"device">>]}
               ,'include_docs'
              ],
    %% TODO: Using the cf_attributes from crossbar isn't exactly kosher
    Context1 = crossbar_doc:load_view(<<"cf_attributes/owned">>, Options, Context),
    {cb_context:doc(Context1), Context1}.

-spec group_summary(cb_context:context(), ne_binary()) -> cb_context:context().
group_summary(Context, GroupId) ->
    {GroupEndpoints, Context1} = group_endpoints(Context, GroupId),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' ->
            get_channels(Context
                         ,GroupEndpoints
                         ,fun wapi_call:publish_query_user_channels_req/1
                        )
    end.

-spec group_endpoints(cb_context:context(), ne_binary()) -> endpoints_return().
group_endpoints(Context, _GroupId) ->
    wh_json:foldl(fun group_endpoints_fold/3
                  ,{[], Context}
                  ,wh_json:get_value(<<"endpoints">>, cb_context:doc(Context), wh_json:new())
                 ).

-spec group_endpoints_fold(ne_binary(), wh_json:object(), endpoints_return()) ->
                                  endpoints_return().
group_endpoints_fold(EndpointId, EndpointData, {Acc, Context}) ->
    case wh_json:get_value(<<"type">>, EndpointData) of
        <<"user">> ->
            {EPs, Context1} = user_endpoints(Context, EndpointId),
            {EPs ++ Acc, Context1};
        <<"device">> ->
            Context1 = crossbar_doc:load(EndpointId, Context, ?TYPE_CHECK_OPTION(kz_device:type())),
            {[cb_context:doc(Context1) | Acc], Context1};
        _Type ->
            lager:debug("skipping type ~s", [_Type]),
            {Acc, Context}
    end.

-spec account_summary(cb_context:context()) -> cb_context:context().
account_summary(Context) ->
    get_channels(Context, [], fun wapi_call:publish_query_account_channels_req/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_channels(cb_context:context(), wh_json:objects(), function()) -> cb_context:context().
get_channels(Context, Devices, PublisherFun) ->
    Realm = wh_util:get_account_realm(cb_context:account_id(Context)),

    Usernames = [Username
                 || JObj <- Devices,
                    (Username = wh_json:get_first_defined(
                                  [[<<"doc">>, <<"sip">>, <<"username">>]
                                   ,[<<"sip">>, <<"username">>]
                                  ]
                                  ,JObj
                                 )
                    )
                        =/= 'undefined'
                ],

    Req = [{<<"Realm">>, Realm}
           ,{<<"Usernames">>, lists:usort(Usernames)} % unique list of usernames
           ,{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Active-Only">>, 'false'}
           ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    case wh_amqp_worker:call_collect(Req
                                     ,PublisherFun
                                     ,{'ecallmgr', 'true'}
                                    )
    of
        {'error', _R} ->
            lager:error("could not reach ecallmgr channels: ~p", [_R]),
            crossbar_util:response('error', <<"could not reach ecallmgr channels">>, Context);
        {_OK, Resp} ->
            Channels = merge_user_channels_jobjs(Resp),
            crossbar_util:response(Channels, Context)
    end.

-spec merge_user_channels_jobjs(wh_json:objects()) -> wh_json:objects().
merge_user_channels_jobjs(JObjs) ->
    merge_user_channels_jobjs(JObjs, dict:new()).

-spec merge_user_channels_jobjs(wh_json:objects(), dict:dict()) -> wh_json:objects().
merge_user_channels_jobjs([], Dict) ->
    [delete_keys(Channel) || {_, Channel} <- dict:to_list(Dict)];
merge_user_channels_jobjs([JObj|JObjs], Dict) ->
    merge_user_channels_jobjs(JObjs, merge_user_channels_jobj(JObj, Dict)).

-spec merge_user_channels_jobj(wh_json:object(), dict:dict()) -> dict:dict().
merge_user_channels_jobj(JObj, Dict) ->
    lists:foldl(fun merge_user_channels_fold/2, Dict, wh_json:get_value(<<"Channels">>, JObj, [])).

-spec merge_user_channels_fold(wh_json:object(), dict:dict()) -> dict:dict().
merge_user_channels_fold(Channel, D) ->
    UUID = wh_json:get_value(<<"uuid">>, Channel),
    dict:store(UUID, Channel, D).

-spec delete_keys(wh_json:object()) -> wh_json:object().
delete_keys(JObj) ->
    wh_json:delete_keys([<<"account_id">>
                         ,<<"bridge_id">>
                         ,<<"context">>
                         ,<<"dialplan">>
                         ,<<"handling_locally">>
                         ,<<"node">>
                         ,<<"precedence">>
                         ,<<"profile">>
                         ,<<"realm">>
                         ,<<"app_name">>
                         ,<<"app_version">>
                         ,<<"event_category">>
                         ,<<"event_name">>
                         ,<<"msg_id">>
                         ,<<"node">>
                         ,<<"server_id">>
                         ,<<"switch_hostname">>
                         ,<<"switch_nodename">>
                         ,<<"switch_url">>
                         ,<<"media_node">>
                         ,<<"fetch_id">>
                        ], JObj).

-spec normalize_channel(wh_json:object()) -> wh_json:object().
normalize_channel(JObj) ->
    delete_keys(
      wh_json:normalize(JObj)
     ).

-spec maybe_transfer(cb_context:context(), ne_binary()) -> cb_context:context().
-spec maybe_transfer(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
-spec maybe_transfer(cb_context:context(), ne_binary(), ne_binary(), ne_binary()) -> cb_context:context().
maybe_transfer(Context, Transferor) ->
    Channel = cb_context:resp_data(Context),
    case wh_json:get_value(<<"other_leg_call_id">>, Channel) of
        'undefined' ->
            lager:debug("no transferee leg found"),
            cb_context:add_validation_error(<<"other_leg_call_id">>
                                           ,<<"required">>
                                           ,wh_json:from_list([{<<"message">>, <<"Channel is not bridged">>}])
                                           ,Context
                                           );
        Transferee ->
            maybe_transfer(Context, Transferor, Transferee)
    end.

maybe_transfer(Context, Transferor, Transferee) ->
    case cb_context:req_value(Context, <<"target">>) of
        'undefined' ->
            lager:debug("no target destination"),
            cb_context:add_validation_error(<<"target">>
                                           ,<<"required">>
                                           ,wh_json:from_list([{<<"message">>, <<"No target destination specified">>}])
                                           ,Context
                                           );
        Target ->
            maybe_transfer(Context, Transferor, Transferee, Target)
    end.

maybe_transfer(Context, Transferor, _Transferee, Target) ->
    API = [{<<"Call-ID">>, Transferor}
          ,{<<"Action">>, <<"transfer">>}
          ,{<<"Data">>, wh_json:from_list(
                          [{<<"target">>, Target}
                          ,{<<"takeback_dtmf">>, cb_context:req_value(Context, <<"takeback_dtmf">>)}
                          ,{<<"moh">>, cb_context:req_value(Context, <<"moh">>)}
                          ])
           }
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    lager:debug("attempting to transfer ~s to ~s by ~s", [_Transferee, Target, Transferor]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1),
    crossbar_util:response_202(<<"transfer initiated">>, Context).

-spec maybe_hangup(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_hangup(Context, CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"hangup">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1),
    crossbar_util:response_202(<<"hangup initiated">>, Context).

-spec maybe_break(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_break(Context, CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"break">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to break ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1),
    crossbar_util:response_202(<<"break initiated">>, Context).

-spec maybe_callflow(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_callflow(Context, CallId) ->
    CallflowId = cb_context:req_value(Context, <<"id">>),
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"callflow">>}
           ,{<<"Data">>, wh_json:from_list(
                           [{<<"id">>, CallflowId}
                            ,{<<"captures">>, cb_context:req_value(Context, <<"captures">>)}
                            ,{<<"collected">>, cb_context:req_value(Context, <<"collected">>)}
                           ])
            }
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    lager:debug("attempting to running callflow ~s on ~s", [CallflowId, CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1),
    crossbar_util:response_202(<<"callflow initiated">>, Context).
