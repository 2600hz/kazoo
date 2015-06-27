%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_sms).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,delete/2
        ]).

-include("../crossbar.hrl").


-define(CB_LIST_ALL, <<"sms/crossbar_listing">>).
-define(CB_LIST_BY_DEVICE, <<"sms/listing_by_device">>).
-define(CB_LIST_BY_OWNERID, <<"sms/listing_by_owner">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".sms">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.sms">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.sms">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.sms">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.sms">>, ?MODULE, 'put'),
    crossbar_bindings:bind(<<"*.execute.delete.sms">>, ?MODULE, 'delete').

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
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) ->
    [?HTTP_GET, ?HTTP_DELETE].



%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /faxes => []
%%    /faxes/foo => [<<"foo">>]
%%    /faxes/foo/bar => [<<"foo">>, <<"bar">>]
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
%% /faxes mights load a list of fax objects
%% /faxes/123 might load the fax object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

validate(Context, Id) ->
    validate_sms(Context, Id, cb_context:req_verb(Context)).

-spec validate_request(cb_context:context(), http_method()) -> cb_context:context().
validate_request(Context, ?HTTP_GET) ->
    summary(Context);
validate_request(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_sms(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_sms(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_sms(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C) end,
    cb_context:validate_request_data(<<"sms">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(<<Year:4/binary, Month:2/binary, "-", _/binary>> = Id, Context) ->
    crossbar_doc:load(Id, cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)));
read(Id, Context) ->
    crossbar_doc:load(Id, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    JObj = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_modb(Context),
    kazoo_modb:create(AccountDb),
    ResellerId = cb_context:reseller_id(Context),
    Realm = wh_util:get_account_realm(AccountId),

    {AuthorizationType, Authorization, OwnerId} =
        case {cb_context:user_id(Context), cb_context:auth_user_id(Context)} of
            {'undefined', 'undefined'} ->
                {<<"account">>, AccountId, 'undefined'};
            {UserId, 'undefined'} ->
                {<<"user">>, UserId, UserId};
            {'undefined', UserAuth} ->
                {<<"user">>, UserAuth, UserAuth};
            {UserId, _UserAuth} ->
                {<<"user">>, UserId, UserId}
        end,

    {ToNum, ToOptions} = build_number(wh_json:get_value(<<"to">>, JObj)),
    ToUser =
        case whapps_account_config:get_global(AccountId, ?MOD_CONFIG_CAT, <<"api_e164_convert_to">>, 'false')
            andalso wnm_util:is_reconcilable(filter_number(ToNum), AccountId)
        of
            'true' -> wnm_util:to_e164(filter_number(ToNum), AccountId);
            'false' -> ToNum
        end,
    To = <<ToUser/binary, "@", Realm/binary>>,

    {FromNum, FromOptions} = build_number(wh_json:get_value(<<"from">>, JObj, get_default_caller_id(Context, OwnerId))),
    FromUser =
        case whapps_account_config:get_global(AccountId, ?MOD_CONFIG_CAT, <<"api_e164_convert_from">>, 'false')
            andalso wnm_util:is_reconcilable(filter_number(FromNum), AccountId)
        of
            'true' -> wnm_util:to_e164(filter_number(FromNum), AccountId);
            'false' -> FromNum
        end,
    From = <<FromUser/binary, "@", Realm/binary>>,

    AddrOpts = [{<<"SMPP-Address-From-", K/binary>>, V} || {K, V} <- FromOptions]
        ++ [{<<"SMPP-Address-To-", K/binary>>, V} || {K, V} <- ToOptions],

    SmsDocId = create_sms_doc_id(),

    cb_context:set_doc(cb_context:set_account_db(Context, AccountDb)
                       ,wh_json:set_values(
                          props:filter_undefined(
                            [{<<"pvt_type">>, <<"sms">>}
                             ,{<<"pvt_status">>, <<"queued">>}
                             ,{<<"pvt_account_id">>, AccountId}
                             ,{<<"pvt_account_db">>, AccountDb}
                             ,{<<"pvt_reseller_id">>, ResellerId}
                             ,{<<"pvt_owner_id">>, OwnerId}
                             ,{<<"pvt_authorization_type">>, AuthorizationType}
                             ,{<<"pvt_authorization">>, Authorization}
                             ,{<<"pvt_origin">>, <<"api">>}
                             ,{<<"pvt_address_options">>, wh_json:from_list(AddrOpts)}
                             ,{<<"request">>, To}
                             ,{<<"request_user">>, ToUser}
                             ,{<<"request_realm">>, Realm}
                             ,{<<"to">>, To}
                             ,{<<"to_user">>, ToUser}
                             ,{<<"to_realm">>, Realm}
                             ,{<<"from">>, From}
                             ,{<<"from_user">>, FromUser}
                             ,{<<"from_realm">>, Realm}
                             ,{<<"_id">>, SmsDocId}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                            ])
                          ,JObj
                         )).

-define(CALLER_ID_INTERNAL, [<<"caller_id">>, <<"internal">>, <<"number">>]).
-define(CALLER_ID_EXTERNAL, [<<"caller_id">>, <<"external">>, <<"number">>]).

-spec get_default_caller_id(cb_context:context(), api_binary()) -> api_binary().
get_default_caller_id(Context, 'undefined') ->
    {'ok', JObj} = kz_account:fetch(cb_context:account_id(Context)),
    wh_json:get_first_defined([?CALLER_ID_INTERNAL, ?CALLER_ID_EXTERNAL], JObj, <<"anonymous">>);
get_default_caller_id(Context, OwnerId) ->
    AccountDb = cb_context:account_db(Context),
    {'ok', JObj1} = kz_account:fetch(AccountDb),
    {'ok', JObj2} = couch_mgr:open_cache_doc(AccountDb, OwnerId),
    wh_json:get_first_defined([?CALLER_ID_INTERNAL, ?CALLER_ID_EXTERNAL]
                              ,wh_json:merge_recursive(JObj1, JObj2)
                              ,<<"anonymous">>
                             ).

-spec create_sms_doc_id() -> ne_binary().
create_sms_doc_id() ->
    {Year, Month, _} = erlang:date(),
    wh_util:to_binary(
      io_lib:format("~B~s-~s",[Year
                               ,wh_util:pad_month(Month)
                               ,wh_util:rand_hex_binary(16)
                              ])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    {View, PreFilter, PostFilter} = get_view_and_filter(Context),
    case cb_modules_util:range_modb_view_options(Context, PreFilter, PostFilter) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(View, ViewOptions, Context, fun normalize_view_results/2);
        Ctx -> Ctx
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [normalize_view_result_value(wh_json:get_value(<<"value">>, JObj))|Acc].

-spec normalize_view_result_value(wh_json:object()) -> wh_json:object().
normalize_view_result_value(JObj) ->
    Date = wh_util:rfc1036(wh_doc:created(JObj)),
    wh_json:set_value(<<"date">>, Date, JObj).

-spec get_view_and_filter(cb_context:context()) ->
                                 {ne_binary(), api_binaries(), api_binaries()}.
get_view_and_filter(Context) ->
    case {cb_context:device_id(Context), cb_context:user_id(Context)} of
        {'undefined', 'undefined'} -> {?CB_LIST_ALL, 'undefined', [wh_json:new()]};
        {'undefined', Id} -> {?CB_LIST_BY_OWNERID, [Id], 'undefined'};
        {Id , 'undefined'} -> {?CB_LIST_BY_DEVICE, [Id], 'undefined'};
        {Id, _} -> {?CB_LIST_BY_DEVICE, [Id], 'undefined'}
    end.

-spec filter_number(binary()) -> binary().
filter_number(Number) ->
    << <<X>> || <<X>> <= Number, is_digit(X)>>.

-spec is_digit(integer()) -> boolean().
is_digit(N) -> N >= $0 andalso N =< $9.

-spec build_number(ne_binary()) -> {api_binary(), wh_proplist()}.
build_number(Number) ->
    N = binary:split(Number, <<",">>, ['global']),
    case length(N) of
        1 -> {Number, []};
        _ -> lists:foldl(fun parse_number/2, {'undefined', []}, N)
    end.

-spec parse_number(ne_binary(), {api_binary(), wh_proplist()}) ->
                          {api_binary(), wh_proplist()}.
parse_number(<<"TON=", N/binary>>, {Num, Options}) ->
    {Num, [{<<"TON">>, wh_util:to_integer(N) } | Options]};
parse_number(<<"NPI=", N/binary>>, {Num, Options}) ->
    {Num, [{<<"NPI">>, wh_util:to_integer(N) } | Options]};
parse_number(N, {_, Options}) ->
    {N, Options}.
