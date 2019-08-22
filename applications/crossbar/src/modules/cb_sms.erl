%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_sms).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,delete/2
        ]).

-include("crossbar.hrl").


-define(CB_LIST_ALL, <<"sms/crossbar_listing">>).
-define(CB_LIST_BY_DEVICE, <<"sms/listing_by_device">>).
-define(CB_LIST_BY_OWNERID, <<"sms/listing_by_owner">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".sms">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.sms">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.sms">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.sms">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.sms">>, ?MODULE, 'put'),
    crossbar_bindings:bind(<<"*.execute.delete.sms">>, ?MODULE, 'delete').

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

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Doc = cb_context:doc(Context),
    case kazoo_modb:save_doc(kz_doc:account_db(Doc), Doc) of
        {'ok', Saved} -> crossbar_util:response(Saved, Context);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, kz_doc:id(Doc), Context)
    end.

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
    cb_context:validate_request_data(kzd_sms:type(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(?MATCH_MODB_PREFIX(Year,Month,_) = Id, Context) ->
    Context1 = cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(kzd_sms:type()));
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_sms:type())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    ContextDoc = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    MODB = cb_context:account_modb(Context),
    ResellerId = cb_context:reseller_id(Context),
    Realm = kzd_accounts:fetch_realm(AccountId),

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

    {ToNum, ToOptions} = build_number(kz_json:get_value(<<"to">>, ContextDoc)),
    ToUser =
        case kapps_account_config:get_global(AccountId, ?MOD_CONFIG_CAT, <<"api_e164_convert_to">>, 'false')
            andalso knm_converters:is_reconcilable(filter_number(ToNum), AccountId)
        of
            'true' -> knm_converters:normalize(filter_number(ToNum), AccountId);
            'false' -> ToNum
        end,

    {FromNum, FromOptions} = build_number(kz_json:get_value(<<"from">>, ContextDoc, get_default_caller_id(Context, OwnerId))),
    FromUser =
        case kapps_account_config:get_global(AccountId, ?MOD_CONFIG_CAT, <<"api_e164_convert_from">>, 'false')
            andalso knm_converters:is_reconcilable(filter_number(FromNum), AccountId)
        of
            'true' -> knm_converters:normalize(filter_number(FromNum), AccountId);
            'false' -> FromNum
        end,

    AddrOpts = [{<<"SMPP-Address-From-", K/binary>>, V} || {K, V} <- FromOptions]
        ++ [{<<"SMPP-Address-To-", K/binary>>, V} || {K, V} <- ToOptions],

    JObj = kz_json:from_list(
             [{<<"_id">>, kazoo_modb_util:modb_id()}
             ,{<<"request">>, <<ToUser/binary, "@", Realm/binary>>}
             ,{<<"request_user">>, ToUser}
             ,{<<"request_realm">>, Realm}
             ,{<<"to">>, <<ToUser/binary, "@", Realm/binary>>}
             ,{<<"to_user">>, ToUser}
             ,{<<"to_realm">>, Realm}
             ,{<<"from">>, <<FromUser/binary, "@", Realm/binary>>}
             ,{<<"from_user">>, FromUser}
             ,{<<"from_realm">>, Realm}
             ,{<<"pvt_status">>, <<"queued">>}
             ,{<<"pvt_reseller_id">>, ResellerId}
             ,{<<"pvt_owner_id">>, OwnerId}
             ,{<<"pvt_authorization_type">>, AuthorizationType}
             ,{<<"pvt_authorization">>, Authorization}
             ,{<<"pvt_origin">>, <<"api">>}
             ,{<<"pvt_address_options">>, kz_json:from_list(AddrOpts)}
             ]
            ),
    Doc = kz_doc:update_pvt_parameters(kz_json:merge(ContextDoc, JObj), MODB, [{'type', kzd_sms:type()}]),
    cb_context:set_doc(cb_context:set_account_db(Context, MODB), Doc).

-define(CALLER_ID_INTERNAL, [<<"caller_id">>, <<"internal">>, <<"number">>]).
-define(CALLER_ID_EXTERNAL, [<<"caller_id">>, <<"external">>, <<"number">>]).

-spec get_default_caller_id(cb_context:context(), kz_term:api_binary()) -> kz_term:api_binary().
get_default_caller_id(Context, 'undefined') ->
    {'ok', JObj} = kzd_accounts:fetch(cb_context:account_id(Context)),
    kz_json:get_first_defined([?CALLER_ID_INTERNAL, ?CALLER_ID_EXTERNAL]
                             ,JObj
                             ,kz_privacy:anonymous_caller_id_number(cb_context:account_id(Context))
                             );
get_default_caller_id(Context, OwnerId) ->
    AccountDb = cb_context:account_db(Context),
    {'ok', JObj1} = kzd_accounts:fetch(AccountDb),
    {'ok', JObj2} = kz_datamgr:open_cache_doc(AccountDb, OwnerId),
    kz_json:get_first_defined([?CALLER_ID_INTERNAL, ?CALLER_ID_EXTERNAL]
                             ,kz_json:merge(JObj1, JObj2)
                             ,kz_privacy:anonymous_caller_id_number(cb_context:account_id(Context))
                             ).

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

-spec filter_number(binary()) -> binary().
filter_number(Number) ->
    << <<X>> || <<X>> <= Number, is_digit(X)>>.

-spec is_digit(integer()) -> boolean().
is_digit(N) when is_integer(N),
                 N >= $0,
                 N =< $9 -> true;
is_digit(_) -> false.

-spec build_number(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:proplist()}.
build_number(Number) ->
    N = binary:split(Number, <<",">>, ['global']),
    case N of
        [_One] -> {Number, []};
        _ -> lists:foldl(fun parse_number/2, {'undefined', []}, N)
    end.

-spec parse_number(kz_term:ne_binary(), {kz_term:api_binary(), kz_term:proplist()}) ->
                          {kz_term:api_binary(), kz_term:proplist()}.
parse_number(<<"TON=", N/binary>>, {Num, Options}) ->
    {Num, [{<<"TON">>, kz_term:to_integer(N) } | Options]};
parse_number(<<"NPI=", N/binary>>, {Num, Options}) ->
    {Num, [{<<"NPI">>, kz_term:to_integer(N) } | Options]};
parse_number(N, {_, Options}) ->
    {N, Options}.
