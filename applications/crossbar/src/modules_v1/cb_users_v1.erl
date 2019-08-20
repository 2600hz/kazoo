%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Users module
%%% Handle client requests for user documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_users_v1).

-export([create_user/1]).
-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate_resource/1, validate_resource/2, validate_resource/3
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2, post/3
        ,delete/2
        ,patch/2
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).
-define(CB_LIST, <<"users/crossbar_listing">>).
-define(CHANNELS, <<"channels">>).
-define(VCARD, <<"vcard">>).
-define(PHOTO, <<"photo">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%% SUPPORT FOR THE DEPRECIATED CB_SIGNUPS...
-spec create_user(cb_context:context()) -> cb_context:context().
create_user(Context) ->
    Context1 = validate_request('undefined', cb_context:set_req_verb(Context, ?HTTP_PUT)),
    case cb_context:resp_status(Context1) of
        'success' -> put(Context1);
        _Status -> Context1
    end.

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.users">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.users">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate_resource.users">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.users">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.users">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.users">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.users">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.patch.users">>, ?MODULE, 'patch'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_, ?CHANNELS) ->
    [?HTTP_GET];
allowed_methods(_, ?PHOTO) ->
    [?HTTP_POST];
allowed_methods(_, ?VCARD) ->
    [?HTTP_GET].

-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
content_types_provided(Context) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _, ?VCARD) ->
    cb_context:set_content_types_provided(Context, [{'to_binary', [{<<"text">>, <<"x-vcard">>}
                                                                  ,{<<"text">>, <<"directory">>}
                                                                  ]
                                                    }
                                                   ]);
content_types_provided(Context, _, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?CHANNELS) -> 'true';
resource_exists(_, ?VCARD) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with userId
%%
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, UserId) -> validate_user_id(UserId, Context).

-spec validate_resource(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_resource(Context, UserId, _) -> validate_user_id(UserId, Context).

-spec validate_user_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_user_id(UserId, Context) ->
    case kz_datamgr:open_cache_doc(cb_context:account_db(Context), UserId) of
        {'ok', Doc} -> validate_user_id(UserId, Context, Doc);
        {'error', 'not_found'} ->
            cb_context:add_system_error('bad_identifier'
                                       ,kz_json:from_list([{<<"cause">>, UserId}])
                                       ,Context
                                       );
        {'error', _R} -> crossbar_util:response_db_fatal(Context)
    end.

-spec validate_user_id(kz_term:api_binary(), cb_context:context(), kz_json:object()) -> cb_context:context().
validate_user_id(UserId, Context, Doc) ->
    case kz_doc:is_soft_deleted(Doc) of
        'true' ->
            cb_context:add_system_error('bad_identifier'
                                       ,kz_json:from_list([{<<"cause">>, UserId}])
                                       ,Context
                                       );
        'false'->
            cb_context:setters(Context
                              ,[{fun cb_context:set_user_id/2, UserId}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    authenticate_users(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

authenticate_users(?USERS_QCALL_NOUNS(_UserId, _Number), ?HTTP_GET) ->
    lager:debug("authenticating request"),
    'true';
authenticate_users(_Nouns, _Verb) -> 'false'.

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    authorize_users(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

authorize_users(?USERS_QCALL_NOUNS(_UserId, _Number), ?HTTP_GET) ->
    lager:debug("authorizing request"),
    'true';
authorize_users(_Nouns, _Verb) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_users(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, UserId) ->
    validate_user(Context, UserId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, UserId, ?CHANNELS) ->
    Options = [{'key', [UserId, <<"device">>]}
              ,'include_docs'
              ],
    Context1 = crossbar_doc:load_view(<<"attributes/owned">>, Options, Context),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' -> get_channels(Context1)
    end;
validate(Context, UserId, ?VCARD) ->
    Context1 = load_user(UserId, Context),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' -> convert_to_vcard(Context1)
    end.

-spec validate_users(cb_context:context(), http_method()) -> cb_context:context().
validate_users(Context, ?HTTP_GET) ->
    load_user_summary(Context);
validate_users(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate_user(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_user(Context, UserId, ?HTTP_GET) ->
    load_user(UserId, Context);
validate_user(Context, UserId, ?HTTP_POST) ->
    validate_request(UserId, Context);
validate_user(Context, UserId, ?HTTP_DELETE) ->
    load_user(UserId, Context);
validate_user(Context, UserId, ?HTTP_PATCH) ->
    validate_patch(UserId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    _ = crossbar_util:maybe_refresh_fs_xml('user', Context),
    Context1 = cb_modules_util:take_sync_field(Context),
    sync_sip_data(Context1),
    crossbar_doc:save(cb_modules_util:remove_plaintext_password(Context1)).

-spec sync_sip_data(cb_context:context()) -> 'ok'.
sync_sip_data(Context) ->
    NewDoc = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),

    case cb_context:fetch(Context, 'sync') of
        'false' -> 'ok';
        'true' -> provisioner_util:sync_user(AccountId);
        'force' -> provisioner_util:force_sync_user(AccountId, NewDoc)
    end.

-spec post(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
post(Context, UserId, ?PHOTO) ->
    [{_FileName, FileObj}] = cb_context:req_files(Context),
    Headers = kz_json:get_value(<<"headers">>, FileObj),
    CT = kz_json:get_value(<<"content_type">>, Headers),
    Content = kz_json:get_value(<<"contents">>, FileObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(kzd_users:type())],
    crossbar_doc:save_attachment(UserId, ?PHOTO, Content, Context, Opts).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_channels(cb_context:context()) -> cb_context:context().
get_channels(Context) ->
    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Usernames = [Username
                 || JObj <- cb_context:doc(Context),
                    Username <- [kzd_devices:sip_username(kz_json:get_value(<<"doc">>, JObj))],
                    Username =/= undefined
                ],
    Req = [{<<"Realm">>, Realm}
          ,{<<"Usernames">>, Usernames}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_call:publish_query_user_channels_req/1
                                    ,{'ecallmgr', 'true'}
                                    )
    of
        {'error', _R} ->
            lager:error("could not reach ecallmgr channels: ~p", [_R]),
            crossbar_util:response('error', <<"could not reach ecallmgr channels">>, Context);
        {_, Resp} ->
            Channels = merge_user_channels_jobjs(Resp),
            crossbar_util:response(Channels, Context)
    end.

%% Card related code should be placed somewhere else.
%% We also want to extend 'lists' to be vcard-compatible.
%% And we want to use it for XMPP (for xCards).
-spec convert_to_vcard(cb_context:context()) -> cb_context:context().
convert_to_vcard(Context) ->
    JObj = cb_context:doc(Context),
    JProfile = kz_json:get_value(<<"profile">>, JObj, kz_json:new()),
    JObj1 = kz_json:merge_jobjs(JObj, JProfile),
    JObj2 = set_photo(JObj1, Context),
    JObj3 = set_org(JObj2, Context),
    %% TODO add SOUND, AGENT (X-ASSISTANT), X-MANAGER
    Fields = [
              <<"BEGIN">>
             ,<<"VERSION">>
             ,<<"FN">>
             ,<<"N">>
             ,<<"ORG">>
             ,<<"PHOTO">>
             ,<<"EMAIL">>
             ,<<"BDAY">>
             ,<<"NOTE">>
             ,<<"TITLE">>
             ,<<"ROLE">>
             ,<<"TZ">>
             ,<<"NICKNAME">>
             ,<<"TEL">>
             ,<<"ADR">>
             ,<<"END">>
             ],
    NotEmptyFields = lists:foldl(fun vcard_fields_acc/2, [], [card_field(Key, JObj3) || Key <- Fields]),
    PackedFields = lists:reverse([iolist_join(<<":">>, [X, Y]) || {X, Y} <- NotEmptyFields]),
    DividedFields = lists:reverse(lists:foldl(fun vcard_field_divide_by_length/2, [], PackedFields)),
    RespData = iolist_join(<<"\n">>, DividedFields),
    cb_context:set_resp_data(Context, [RespData, <<"\n">>]).

-spec set_photo(kz_json:object(), cb_context:context()) -> kz_json:object().
set_photo(JObj, Context) ->
    UserId = kz_doc:id(cb_context:doc(Context)),
    Attach = crossbar_doc:load_attachment(UserId, ?PHOTO, ?TYPE_CHECK_OPTION(kzd_users:type()), Context),
    case cb_context:resp_status(Attach) of
        'error' -> JObj;
        'success' ->
            Data = cb_context:resp_data(Attach),
            CT = kz_doc:attachment_content_type(cb_context:doc(Context), ?PHOTO),
            kz_json:set_value(?PHOTO, kz_json:from_list([{CT, Data}]), JObj)
    end.

-spec set_org(kz_json:object(), cb_context:context()) -> kz_json:object().
set_org(JObj, Context) ->
    LoadedContent = crossbar_doc:load(cb_context:account_id(Context)
                                     ,Context
                                     ,?TYPE_CHECK_OPTION(kzd_users:type())
                                     ),

    case 'success' =:= cb_context:resp_status(LoadedContent)
        andalso kz_json:get_value(<<"org">>, cb_context:doc(LoadedContent))
    of
        'false' -> JObj;
        'undefined' -> JObj;
        Val -> kz_json:set_value(<<"org">>, Val, JObj)
    end.

-spec vcard_escape_chars(binary()) -> binary().
vcard_escape_chars(Val) ->
    Val1 = re:replace(Val, "(:|;|,)", "\\\\&", [global, {return, binary}]),
    re:replace(Val1, "\n", "\\\\n", [global, {return, binary}]).

vcard_fields_acc({_, Val}, Acc) when Val =:= 'undefined'; Val =:= []; Val =:= <<>> ->
    Acc;
vcard_fields_acc({Type, Val}, Acc) ->
    case vcard_normalize_val(Val) of
        <<>> -> Acc;
        ValN ->
            TypeN = vcard_normalize_type(Type),
            [{TypeN, ValN} | Acc]
    end;
vcard_fields_acc([X | Rest], Acc) ->
    vcard_fields_acc(Rest, vcard_fields_acc(X, Acc));
vcard_fields_acc([], Acc) ->
    Acc.

-spec vcard_normalize_val(binary() | {char(), kz_term:binaries()}) -> binary().
vcard_normalize_val({Divider, Vals}) when is_list(Vals) ->
    iolist_join(Divider, [vcard_escape_chars(X) || X <- Vals, X =/= 'undefined', X =/= [], X =/= <<>>]);
vcard_normalize_val(Val) when is_binary(Val) ->
    vcard_escape_chars(Val).

-spec vcard_normalize_type(list() | {kz_term:ne_binary(), kz_term:ne_binary()} | kz_term:ne_binary()) -> kz_term:ne_binary().
vcard_normalize_type(T) when is_list(T) -> iolist_join(<<";">>, [vcard_normalize_type(X) || X <- T]);
vcard_normalize_type({T, V}) -> iolist_join(<<"=">>, [T, V]);
vcard_normalize_type(T) -> T.

%%-spec card_field(kz_term:ne_binary(), kz_json:object()) -> {vcard_type_spec(), vcard_val()}.
card_field(Key, _)
  when Key =:= <<"BEGIN">> ->
    {Key, <<"VCARD">>};
card_field(Key, _)
  when Key =:= <<"VERSION">> ->
    {Key, <<"3.0">>};
card_field(Key, _)
  when Key =:= <<"END">> ->
    {Key, <<"VCARD">>};
card_field(Key, JObj)
  when Key =:= <<"FN">> ->
    FirstName = kz_json:get_value(<<"first_name">>, JObj),
    LastName = kz_json:get_value(<<"last_name">>, JObj),
    MiddleName = kz_json:get_value(<<"middle_name">>, JObj),
    {Key, iolist_join(<<" ">>, [X || X <- [FirstName, MiddleName, LastName], X =/= undefined, X =/= [], X =/= <<>>])};
card_field(Key, JObj) when Key =:= <<"N">> ->
    FirstName = kz_json:get_value(<<"first_name">>, JObj),
    LastName = kz_json:get_value(<<"last_name">>, JObj),
    MiddleName = kz_json:get_value(<<"middle_name">>, JObj),
    {Key, {$;, [LastName, FirstName, MiddleName]}};
card_field(Key, JObj) when Key =:= <<"ORG">> ->
    {Key, element(2, card_field(<<"org">>, JObj))};
card_field(Key, JObj) when Key =:= <<"PHOTO">> ->
    case card_field(?PHOTO, JObj) of
        {?PHOTO, 'undefined'} -> {Key, 'undefined'};
        {?PHOTO, PhotoJObj} ->
            [{CT, PhotoBin}] = kz_json:to_proplist(PhotoJObj),
            <<"image/jpeg">> = CT,
            TypeType = <<"JPEG">>,
            Data = base64:encode(PhotoBin),
            {[Key, {<<"ENCODING">>, <<"B">>}, {<<"TYPE">>, TypeType}], Data}
    end;
card_field(Key, JObj) when Key =:= <<"ADR">> ->
    Addresses = kz_json:get_value(<<"addresses">>, JObj, []),
    [{Key, X} || X <- Addresses];
card_field(Key, JObj) when Key =:= <<"TEL">> ->
    CallerId = kz_json:get_value(<<"caller_id">>, JObj, kz_json:new()),
    Internal = kz_json:get_value(<<"internal">>, CallerId),
    External = kz_json:get_value(<<"external">>, CallerId),
    [
     {Key, Internal}
    ,{Key, External}
    ];
card_field(Key = <<"EMAIL">>, JObj) ->
    {Key, element(2, card_field(<<"email">>, JObj))};
card_field(Key = <<"BDAY">>, JObj) ->
    {Key, element(2, card_field(<<"birthday">>, JObj))};
card_field(Key = <<"NOTE">>, JObj) ->
    {Key, element(2, card_field(<<"note">>, JObj))};
card_field(Key = <<"TITLE">>, JObj) ->
    {Key, element(2, card_field(<<"title">>, JObj))};
card_field(Key = <<"ROLE">>, JObj) ->
    {Key, element(2, card_field(<<"role">>, JObj))};
card_field(Key = <<"TZ">>, JObj) ->
    {Key, element(2, card_field(<<"timezone">>, JObj))};
card_field(Key = <<"NICKNAME">>, JObj) ->
    Val = case element(2, card_field(<<"nicknames">>, JObj)) of
              'undefined' -> [];
              V -> V
          end,
    {Key, {$,, Val}};
card_field(Key, JObj) ->
    {Key, kz_json:get_value(Key, JObj)}.

-spec iolist_join(char() | binary(), kz_term:binaries()) -> binary().
iolist_join(Divider, List) ->
    case lists:foldl(fun iolist_join_acc/2, {Divider, []}, List) of
        {_, [_ | Reversed]} -> iolist_to_binary(lists:reverse(Reversed));
        {_, []} -> <<>>
    end.

-spec iolist_join_acc(binary(), {char() | binary(), kz_term:binaries()}) -> {char() | binary(), kz_term:binaries()}.
iolist_join_acc(E, {Divider, Acc}) ->
    {Divider, [Divider, E | Acc]}.

-spec vcard_field_divide_by_length(binary(), kz_term:binaries()) -> kz_term:binaries().
vcard_field_divide_by_length(<<Row:75/binary, Rest/binary>>, Acc) ->
    vcard_field_divide_by_length(Rest, [Row | Acc]);
vcard_field_divide_by_length(Row, Acc) ->
    [Row | Acc].

-spec merge_user_channels_jobjs(kz_json:objects()) -> kz_json:objects().
merge_user_channels_jobjs(JObjs) ->
    merge_user_channels_jobjs(JObjs, dict:new()).

-spec merge_user_channels_jobjs(kz_json:objects(), dict:dict()) -> kz_json:objects().
merge_user_channels_jobjs([], Dict) ->
    [Channel || {_, Channel} <- dict:to_list(Dict)];
merge_user_channels_jobjs([JObj|JObjs], Dict) ->
    merge_user_channels_jobjs(JObjs, merge_user_channels_jobj(JObj, Dict)).

-spec merge_user_channels_jobj(kz_json:object(), dict:dict()) -> dict:dict().
merge_user_channels_jobj(JObj, Dict) ->
    lists:foldl(fun merge_user_channels_fold/2, Dict, kz_json:get_value(<<"Channels">>, JObj, [])).

-spec merge_user_channels_fold(kz_json:object(), dict:dict()) -> dict:dict().
merge_user_channels_fold(Channel, D) ->
    UUID = kz_json:get_value(<<"uuid">>, Channel),
    dict:store(UUID, Channel, D).

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_user_summary(cb_context:context()) -> cb_context:context().
load_user_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Load a user document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_user(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
load_user(UserId, Context) -> crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(UserId, Context) ->
    prepare_username(UserId, Context).

-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(UserId, Context) ->
    crossbar_doc:patch_and_validate(UserId, Context, fun validate_request/2).

-spec prepare_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_username(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    case kz_json:get_ne_value(<<"username">>, JObj) of
        'undefined' -> check_user_name(UserId, Context);
        Username ->
            JObj1 = kz_json:set_value(<<"username">>, kz_term:to_lower_binary(Username), JObj),
            check_user_name(UserId, cb_context:set_req_data(Context, JObj1))
    end.

-spec check_user_name(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_user_name(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    UserName = kz_json:get_ne_value(<<"username">>, JObj),
    AccountDb = cb_context:account_db(Context),
    case is_username_unique(AccountDb, UserId, UserName) of
        'true' ->
            lager:debug("user name ~s is unique", [UserName]),
            check_emergency_caller_id(UserId, Context);
        'false' ->
            lager:error("user name ~s is already in use", [UserName]),
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"User name already in use">>}
                    ,{<<"cause">>, UserName}
                    ]),
            Context1 = cb_context:add_validation_error([<<"username">>], <<"unique">>, Msg, Context),
            check_emergency_caller_id(UserId, Context1)
    end.

-spec check_emergency_caller_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(UserId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_user_schema(UserId, Context1).

-spec is_username_unique(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_username_unique(AccountDb, UserId, UserName) ->
    ViewOptions = [{'key', UserName}],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= UserId;
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?LIST_BY_USERNAME, AccountDb]),
            'false'
    end.

-spec check_user_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_user_schema(UserId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(UserId, C) end,
    cb_context:validate_request_data(<<"users">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"user">>}],
    maybe_import_credintials('undefined'
                            ,cb_context:set_doc(Context
                                               ,kz_json:set_values(Props, cb_context:doc(Context))
                                               )
                            );
on_successful_validation(UserId, Context) ->
    maybe_import_credintials(UserId, crossbar_doc:load_merge(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type()))).

-spec maybe_import_credintials(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_import_credintials(UserId, Context) ->
    JObj = cb_context:doc(Context),
    case kz_json:get_ne_value(<<"credentials">>, JObj) of
        'undefined' -> maybe_validate_username(UserId, Context);
        Creds ->
            RemoveKeys = [<<"credentials">>, <<"pvt_sha1_auth">>],
            C = cb_context:set_doc(Context
                                  ,kz_json:set_value(<<"pvt_md5_auth">>, Creds
                                                    ,kz_json:delete_keys(RemoveKeys, JObj)
                                                    )
                                  ),
            maybe_validate_username(UserId, C)
    end.

-spec maybe_validate_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_validate_username(UserId, Context) ->
    NewUsername = kz_json:get_ne_value(<<"username">>, cb_context:doc(Context)),
    CurrentUsername = case cb_context:fetch(Context, 'db_doc') of
                          'undefined' -> NewUsername;
                          CurrentJObj ->
                              kz_json:get_ne_value(<<"username">>, CurrentJObj, NewUsername)
                      end,
    case kz_term:is_empty(NewUsername)
        orelse CurrentUsername =:= NewUsername
        orelse username_doc_id(NewUsername, Context)
    of
        %% user name is unchanged
        'true' -> maybe_rehash_creds(UserId, NewUsername, Context);
        %% updated user name that doesn't exist
        'undefined' ->
            manditory_rehash_creds(UserId, NewUsername, Context);
        %% updated user name to existing, collect any further errors...
        _Else ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"User name is not unique for this account">>}
                    ,{<<"cause">>, NewUsername}
                    ]),
            C = cb_context:add_validation_error(<<"username">>, <<"unique">>, Msg, Context),
            manditory_rehash_creds(UserId, NewUsername, C)
    end.

-spec maybe_rehash_creds(kz_term:api_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_rehash_creds(UserId, Username, Context) ->
    case kz_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        %% No user name or hash, no creds for you!
        'undefined' when Username =:= 'undefined' ->
            HashKeys = [<<"pvt_md5_auth">>, <<"pvt_sha1_auth">>],
            cb_context:set_doc(Context, kz_json:delete_keys(HashKeys, cb_context:doc(Context)));
        %% User name without password, creds status quo
        'undefined' -> Context;
        %% Got a password, hope you also have a user name...
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec manditory_rehash_creds(kz_term:api_binary(), kz_term:api_binary(), cb_context:context()) ->
                                    cb_context:context().
manditory_rehash_creds(UserId, Username, Context) ->
    case kz_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        'undefined' ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"The password must be provided when updating the user name">>}
                    ]),
            cb_context:add_validation_error(<<"password">>, <<"required">>, Msg, Context);
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec rehash_creds(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) ->
                          cb_context:context().
rehash_creds(_UserId, 'undefined', _Password, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"The user name must be provided when updating the password">>}
            ]),
    cb_context:add_validation_error(<<"username">>, <<"required">>, Msg, Context);
rehash_creds(_UserId, Username, Password, Context) ->
    lager:debug("password set on doc, updating hashes for ~s", [Username]),
    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),
    JObj1 = kz_json:set_values([{<<"pvt_md5_auth">>, MD5}
                               ,{<<"pvt_sha1_auth">>, SHA1}
                               ], cb_context:doc(Context)),
    crossbar_auth:reset_identity_secret(
      cb_context:set_doc(Context, kz_json:delete_key(<<"password">>, JObj1))
     ).

%%------------------------------------------------------------------------------
%% @doc This function will determine if the username in the request is
%% unique or belongs to the request being made
%% @end
%%------------------------------------------------------------------------------
-spec username_doc_id(kz_term:api_binary(), cb_context:context()) -> kz_term:api_binary().
username_doc_id(Username, Context) ->
    username_doc_id(Username, Context, cb_context:account_db(Context)).
username_doc_id(_, _, 'undefined') ->
    'undefined';
username_doc_id(Username, Context, _AccountDb) ->
    Username = kz_term:to_lower_binary(Username),
    Context1 = crossbar_doc:load_view(?LIST_BY_USERNAME, [{'key', Username}], Context),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso cb_context:doc(Context1)
    of
        [JObj] -> kz_doc:id(JObj);
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec(normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects()).
normalize_view_results(JObj, Acc) -> [kz_json:get_value(<<"value">>, JObj)|Acc].
