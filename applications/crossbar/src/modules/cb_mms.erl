%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_mms).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,delete/2
        ]).

-include("crossbar.hrl").

-type mime_tuple() :: mimemail:mimetuple().

-define(CB_LIST_ALL, <<"mms/crossbar_listing">>).
-define(CB_LIST_BY_DEVICE, <<"mms/listing_by_device">>).
-define(CB_LIST_BY_OWNERID, <<"mms/listing_by_owner">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".mms">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.mms">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.mms">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.mms">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.mms">>, ?MODULE, 'put'),
    crossbar_bindings:bind(<<"*.execute.delete.mms">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_SMSId) ->
    [?HTTP_GET, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /faxes => []
%%    /faxes/foo => [<<"foo">>]
%%    /faxes/foo/bar => [<<"foo">>, <<"bar">>]
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
%% /faxes might load a list of fax objects
%% /faxes/123 might load the fax object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_mms(Context, Id, cb_context:req_verb(Context)).

-spec validate_request(cb_context:context(), http_method()) -> cb_context:context().
validate_request(Context, ?HTTP_GET) ->
    summary(Context);
validate_request(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_mms(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_mms(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_mms(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Payload = cb_context:doc(Context),
    Ctx = case kz_im:route_type(Payload) of
              <<"onnet">> ->
                  kz_amqp_worker:cast(Payload, fun kapi_im:publish_inbound/1),
                  Context;
              <<"offnet">> ->
                  kz_amqp_worker:cast(Payload, fun kapi_im:publish_outbound/1),
                  AccountId = cb_context:account_id(Context),
                  Rate = kz_services_im:flat_rate(AccountId, 'mms', 'outbound'),
                  Charges = kz_json:from_list([{<<"charges">>, Rate}]),
                  cb_context:set_resp_envelope(Context, kz_json:merge(cb_context:resp_envelope(Context), Charges))
          end,
    crossbar_util:response(kz_json:normalize(kz_im:set_body(kz_api:remove_defaults(Payload), 'null')), Ctx).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C) end,
    cb_context:validate_request_data(kzd_mms:type(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(?MATCH_MODB_PREFIX(Year,Month,_) = Id, Context) ->
    Context1 = cb_context:set_db_name(Context
                                     ,kzs_util:format_account_id(cb_context:account_id(Context)
                                                                ,kz_term:to_integer(Year)
                                                                ,kz_term:to_integer(Month)
                                                                )
                                     ),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(kzd_mms:type()));
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_mms:type())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    Setters = [fun body_or_attachment/1
              ,fun body_is_mime_encoded/1
              ,fun body_from_files/1
              ,fun account_is_enabled/1
              ,fun account_has_mms_enabled/1
              ,fun account_is_in_good_standing/1
              ,fun reseller_has_mms_enabled/1
              ,fun reseller_is_in_good_standing/1
              ,fun create_request/1
              ,fun validate_from/1
              ],
    cb_context:validators(Context, Setters).

-spec create_request(cb_context:context()) -> cb_context:context().
create_request(Context) ->
    Payload = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    ResellerId = cb_context:reseller_id(Context),

    {AuthorizationType, Authorization, OwnerId} =
        case {cb_context:user_id(Context), cb_context:auth_user_id(Context)} of
            {'undefined', 'undefined'} ->
                {<<"account">>, AccountId, 'undefined'};
            {UserId, 'undefined'} ->
                {<<"user">>, UserId, UserId};
            {'undefined', UserAuth} when AuthAccountId =:= AccountId ->
                {<<"user">>, UserAuth, UserAuth};
            {'undefined', _UserAuth} when AuthAccountId =/= AccountId ->
                {<<"account">>, AccountId, 'undefined'};
            {UserId, _UserAuth} ->
                {<<"user">>, UserId, UserId}
        end,

    To = kz_json:get_value(<<"to">>, Payload),
    {Type, ToNum} = case knm_converters:is_reconcilable(To) of
                        'true' -> {<<"offnet">>, knm_converters:normalize(To, AccountId)};
                        'false' -> {<<"onnet">>, To}
                    end,

    FromNum = kz_json:get_value(<<"from">>, Payload, get_default_caller_id(Context, Type, OwnerId)),

    CCVs = [{<<"Account-ID">>, AccountId}
           ,{<<"Reseller-ID">>, ResellerId}
           ,{<<"Authorizing-Type">>, AuthorizationType}
           ,{<<"Authorizing-ID">>, Authorization}
           ,{<<"Owner-ID">>, OwnerId}
           ],

    KVs = [{<<"Message-ID">>, cb_context:req_id(Context)}
          ,{<<"From">>, FromNum}
          ,{<<"To">>, ToNum}
          ,{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Custom-Vars">>, kz_json:from_list(CCVs)}
          ,{<<"Route-Type">>, Type}
          ,{<<"Event-Category">>, <<"mms">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    cb_context:set_doc(Context, kz_json:set_values(KVs, Payload)).

-spec get_default_caller_id(cb_context:context(), binary(), kz_term:api_binary()) -> kz_term:api_binary().
get_default_caller_id(Context, <<"offnet">>, 'undefined') ->
    {'ok', EP} = kz_endpoint:get(cb_context:account_id(Context), cb_context:account_id(Context)),
    kzd_caller_id:external(kzd_accounts:caller_id(EP, kz_json:new()));
get_default_caller_id(Context, <<"onnet">>, 'undefined') ->
    {'ok', EP} = kz_endpoint:get(cb_context:account_id(Context), cb_context:account_id(Context)),
    kzd_caller_id:internal(kzd_accounts:caller_id(EP, kz_json:new()));
get_default_caller_id(Context, <<"offnet">>, OwnerId) ->
    {'ok', EP} = kz_endpoint:get(OwnerId, cb_context:account_id(Context)),
    kzd_caller_id:external(kzd_accounts:caller_id(EP, kz_json:new()));
get_default_caller_id(Context, <<"onnet">>, OwnerId) ->
    {'ok', EP} = kz_endpoint:get(OwnerId, cb_context:account_id(Context)),
    kzd_caller_id:internal(kzd_accounts:caller_id(EP, kz_json:new())).


-spec body_or_attachment(cb_context:context()) -> cb_context:context().
body_or_attachment(Context) ->
    case cb_context:req_files(Context) =:= []
        andalso kzd_mms:body(cb_context:doc(Context)) =:= 'undefined'
    of
        'true' ->
            Property = <<"body">>,
            Code = <<"required">>,
            Message = <<"add mime encoded body or upload one or more files">>,
            cb_context:add_validation_error(Property, Code, Message, Context);
        'false' -> Context
    end.

-spec body_is_mime_encoded(cb_context:context()) -> cb_context:context().
body_is_mime_encoded(Context) ->
    case kzd_mms:body(cb_context:doc(Context)) of
        'undefined' -> Context;
        Body ->
            try mimemail:decode(Body) of
                {_Type, _SubType, _Headers, _Parameters, _Body} ->
                    KVs = [{<<"Body">>, Body}
                          ,{<<"body">>, null}
                          ],
                    cb_context:set_doc(Context, kz_json:set_values(KVs, cb_context:doc(Context)))
            catch
                _What:_Why:_ST ->
                    lager:debug("body decoding failed with ~p:~p", [_What, _Why]),
                    Property = <<"body">>,
                    Code = <<"required">>,
                    Message = <<"add mime encoded body or upload one or more files">>,
                    cb_context:add_validation_error(Property, Code, Message, Context)
            end
    end.

-spec body_from_files(cb_context:context()) -> cb_context:context().
body_from_files(Context) ->
    case cb_context:req_files(Context) of
        [] -> Context;
        Files -> body_from_files(Context, Files)
    end.

-spec body_from_files(cb_context:context(), req_files()) -> cb_context:context().
body_from_files(Context, Files) ->
    Parts = [mimepart_from_file(File)|| File <- Files],
    Encoded = mimemail:encode_part({<<"multipart">>, <<"related">>, [], [], Parts}),
    Body = kz_binary:join(Encoded, <<"\r\n">>),
    cb_context:set_doc(Context, kz_im:set_body(cb_context:doc(Context), Body)).

-spec mimepart_from_file(req_file()) -> mime_tuple().
mimepart_from_file({Filename, FileJObj}) ->
    ContentType = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Headers = [{<<"Content-Type">>, <<ContentType/binary, "; name=\"", Filename/binary, "\"">>}
              ,{<<"Content-Disposition">>, <<"attachment; filename=\"", Filename/binary, "\"">>}
              ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
              ],
    Content = kz_json:get_value(<<"contents">>, FileJObj),
    [Type, SubType] = binary:split(ContentType, <<"/">>),
    {Type, SubType, Headers, [], Content}.

-spec account_is_enabled(cb_context:context()) -> cb_context:context().
account_is_enabled(Context) ->
    case kzd_accounts:enabled(cb_context:account_doc(Context)) of
        'true' -> Context;
        'false' -> cb_context:add_system_error('disabled', Context)
    end.

-spec account_is_in_good_standing(cb_context:context()) -> cb_context:context().
account_is_in_good_standing(Context) ->
    case kz_services_standing:acceptable(cb_context:account_id(Context)) of
        {'true', _} -> Context;
        {'false', #{message := Msg}} -> cb_context:add_system_error('account', Msg, Context)
    end.

-spec account_has_mms_enabled(cb_context:context()) -> cb_context:context().
account_has_mms_enabled(Context) ->
    case kz_services_im:is_mms_enabled(cb_context:account_id(Context)) of
        'true' -> Context;
        'false' -> cb_context:add_system_error('account', <<"mms services not enabled for account">>, Context)
    end.

-spec reseller_is_in_good_standing(cb_context:context()) -> cb_context:context().
reseller_is_in_good_standing(Context) ->
    case kz_services_standing:acceptable(cb_context:reseller_id(Context)) of
        {'true', _} -> Context;
        {'false', #{message := Msg}} ->
            lager:warning("reseller ~s for account ~s is not in good standing => ~p"
                         ,[cb_context:reseller_id(Context)
                          ,cb_context:account_id(Context)
                          ,Msg
                          ]
                         ),
            cb_context:add_system_error('account', <<"service temporarily unavailable">>, Context)
    end.

-spec reseller_has_mms_enabled(cb_context:context()) -> cb_context:context().
reseller_has_mms_enabled(Context) ->
    case kz_services_im:is_sms_enabled(cb_context:reseller_id(Context)) of
        'true' -> Context;
        'false' ->
            lager:warning("mms services not enabled for reseller ~s of account ~s"
                         ,[cb_context:reseller_id(Context)
                          ,cb_context:account_id(Context)
                          ]
                         ),
            cb_context:add_system_error('account', <<"service temporarily unavailable">>, Context)
    end.

-spec validate_from(cb_context:context()) -> cb_context:context().
validate_from(Context) ->
    case kz_im:route_type(cb_context:doc(Context)) of
        <<"onnet">> ->
            Context;
        <<"offnet">> ->
            Setters = [{fun number_exists/2, kz_im:from(cb_context:doc(Context))}
                      ,fun number_belongs_to_account/1
                      ,fun number_has_mms_enabled/1
                      ],
            cb_context:validators(Context, Setters)
    end.

-spec number_exists(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
number_exists(Context, Number) ->
    case knm_phone_number:fetch(Number) of
        {'error', _R} -> cb_context:add_validation_error(<<"from">>, <<"invalid">>, <<"number is invalid">>, Context);
        {'ok', KNumber} -> cb_context:store(Context, 'from_number', KNumber)
    end.

-spec number_belongs_to_account(cb_context:context()) -> cb_context:context().
number_belongs_to_account(Context) ->
    Number = cb_context:fetch(Context, 'from_number'),
    AccountId = cb_context:account_id(Context),
    case knm_phone_number:assigned_to(Number) =:= AccountId of
        'true' -> Context;
        'false' -> cb_context:add_validation_error(<<"from">>, <<"forbidden">>, <<"number does not belong to account">>, Context)
    end.

-spec number_has_mms_enabled(cb_context:context()) -> cb_context:context().
number_has_mms_enabled(Context) ->
    Number = cb_context:fetch(Context, 'from_number'),
    case knm_im:enabled(Number, 'mms') of
        'true' -> Context;
        'false' -> cb_context:add_validation_error(<<"from">>, <<"forbidden">>, <<"number does not have mms enabled">>, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    {ViewName, Opts} =
        build_view_name_range_keys(cb_context:device_id(Context), cb_context:user_id(Context)),
    Options = [{'mapper', fun normalize_view_results/2}
               | Opts
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec build_view_name_range_keys(kz_term:api_binary(), kz_term:api_binary()) -> {kz_term:ne_binary(), crossbar_view:options()}.
build_view_name_range_keys('undefined', 'undefined') ->
    {?CB_LIST_ALL
    ,[{'range_start_keymap', []}
     ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
     ]
    };
build_view_name_range_keys('undefined', Id) ->
    {?CB_LIST_BY_OWNERID
    ,[{'range_keymap', [Id]}]
    };
build_view_name_range_keys(Id, _) ->
    {?CB_LIST_BY_DEVICE
    ,[{'range_keymap', [Id]}]
    }.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    ValueJObj = kz_json:get_value(<<"value">>, JObj),
    Date = kz_time:rfc1036(kz_json:get_value(<<"created">>, ValueJObj)),
    [kz_json:set_value(<<"date">>, Date, JObj) | Acc].
