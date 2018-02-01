%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz INC
%%% @doc
%%% Helpers for manipulating the #cb_context{} record
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_context).

-export([store/3
        ,fetch/2, fetch/3
        ,put_reqid/1
        ,import_errors/1, failed/2
        ,response/1
        ,has_errors/1
        ,add_system_error/2, add_system_error/3, add_system_error/4
        ,add_validation_error/4
        ,validate_request_data/2, validate_request_data/3, validate_request_data/4
        ,add_content_types_provided/2
        ,add_content_types_accepted/2
        ,add_attachment_content_type/3

        ,is_context/1

        ,is_authenticated/1

        ,is_superduper_admin/1
        ,is_account_admin/1

        ,system_error/2

         %% Getters / Setters
        ,setters/2
        ,new/0

        ,account_id/1, set_account_id/2
        ,account_name/1, set_account_name/2
        ,account_db/1, set_account_db/2
        ,user_id/1, set_user_id/2
        ,device_id/1, set_device_id/2
        ,account_modb/1, account_modb/2, account_modb/3
        ,set_account_modb/3, set_account_modb/4
        ,reseller_id/1, set_reseller_id/2
        ,account_realm/1
        ,account_doc/1
        ,auth_token_type/1, set_auth_token_type/2
        ,auth_token/1, set_auth_token/2
        ,auth_doc/1, set_auth_doc/2
        ,auth_account_id/1, set_auth_account_id/2
        ,auth_account_doc/1
        ,auth_user_id/1
        ,req_verb/1, set_req_verb/2
        ,req_data/1, set_req_data/2
        ,req_id/1, set_req_id/2
        ,req_files/1, set_req_files/2
        ,req_nouns/1, set_req_nouns/2
        ,req_headers/1, set_req_headers/2
        ,req_header/2, set_req_header/3
        ,query_string/1, set_query_string/2
        ,client_ip/1, set_client_ip/2
        ,doc/1, set_doc/2, update_doc/2
        ,load_merge_bypass/1, set_load_merge_bypass/2
        ,start/1, set_start/2
        ,pretty_print/1
        ,resp_file/1, set_resp_file/2
        ,resp_data/1, set_resp_data/2
        ,resp_status/1, set_resp_status/2
        ,resp_expires/1, set_resp_expires/2
        ,api_version/1, set_api_version/2
        ,resp_etag/1, set_resp_etag/2
        ,resp_envelope/1, set_resp_envelope/2
        ,allow_methods/1, set_allow_methods/2
        ,allowed_methods/1, set_allowed_methods/2
        ,method/1, set_method/2
        ,path_tokens/1
        ,magic_pathed/1, set_magic_pathed/2
        ,should_soft_delete/1
        ,should_paginate/1, set_should_paginate/2
        ,pagination_page_size/0, pagination_page_size/1

        ,req_json/1, set_req_json/2
        ,resp_error_code/1, set_resp_error_code/2
        ,resp_error_msg/1, set_resp_error_msg/2

        ,resp_headers/1
        ,set_resp_headers/2, set_resp_header/3
        ,add_resp_headers/2, add_resp_header/3

        ,content_types_accepted/1, set_content_types_accepted/2
        ,content_types_provided/1, set_content_types_provided/2
        ,languages_provided/1, set_languages_provided/2
        ,encodings_provided/1, set_encodings_provided/2

        ,validation_errors/1, set_validation_errors/2

        ,host_url/1, set_host_url/2
        ,set_pretty_print/2
        ,set_port/2
        ,set_raw_path/2
        ,raw_qs/1, set_raw_qs/2
        ,profile_id/1 ,set_profile_id/2

         %% Special accessors
        ,req_value/2, req_value/3
        ,accepting_charges/1, set_accepting_charges/1
        ,req_param/2, req_param/3
        ]).

-include("crossbar.hrl").

-define(KEY_ACCEPT_CHARGES, <<"accept_charges">>).

-define(SHOULD_ENSURE_SCHEMA_IS_VALID
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"ensure_valid_schema">>, true)
       ).

-define(SHOULD_FAIL_ON_INVALID_DATA
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"schema_strict_validation">>, false)
       ).

-define(PAGINATION_PAGE_SIZE
       ,kapps_config:get_pos_integer(?CONFIG_CAT, <<"pagination_page_size">>, 50)
       ).

-type context() :: #cb_context{}.
-type setter_fun_1() :: fun((context()) -> context()).
-type setter_fun_2() :: fun((context(), any()) -> context()).
-type setter_fun_3() :: fun((context(), any(), any()) -> context()).
-type setter_fun() :: setter_fun_1() | setter_fun_2() | setter_fun_3().
-export_type([context/0
             ,setter_fun/0
             ,setter_fun_1/0
             ,setter_fun_2/0
             ,setter_fun_3/0
             ]).

-type setter_kv() :: setter_fun_1() |
                     {setter_fun_2(), any()} |
                     {setter_fun_3(), any(), any()}.
-type setters() :: [setter_kv()].

-spec new() -> context().
new() -> #cb_context{}.

-spec is_context(any()) -> boolean().
is_context(#cb_context{}) -> 'true';
is_context(_) -> 'false'.

-spec req_value(context(), kz_json:path()) -> kz_json:api_json_term().
req_value(#cb_context{}=Context, Key) ->
    req_value(Context, Key, 'undefined').

-spec req_value(context(), kz_json:path(), Default) -> kz_json:json_term() | Default.
req_value(#cb_context{req_data=ReqData
                     ,query_json=QS
                     ,req_json=JObj
                     }
         ,Key
         ,Default
         ) ->
    kz_json:find(Key, [ReqData, QS, JObj], Default).

-spec accepting_charges(context()) -> boolean().
accepting_charges(Context) ->
    kz_term:is_true(req_value(Context, ?KEY_ACCEPT_CHARGES, 'false')).

-spec set_accepting_charges(context()) -> context().
set_accepting_charges(#cb_context{req_json = ReqJObj} = Context) ->
    NewReqJObj = kz_json:set_value(?KEY_ACCEPT_CHARGES, 'true', ReqJObj),
    set_req_json(Context, NewReqJObj).

%% Accessors


-spec account_id(context()) -> kz_term:api_ne_binary().
account_id(#cb_context{account_id=AcctId}) -> AcctId.

-spec account_name(context()) -> kz_term:api_ne_binary().
account_name(#cb_context{account_name=Name}) -> Name.

-spec user_id(context()) -> kz_term:api_ne_binary().
user_id(#cb_context{user_id=UserId}) -> UserId.

-spec device_id(context()) -> kz_term:api_ne_binary().
device_id(#cb_context{device_id=DeviceId}) -> DeviceId.

-spec reseller_id(context()) -> kz_term:api_ne_binary().
reseller_id(#cb_context{reseller_id=AcctId}) -> AcctId.

-spec account_db(context()) -> kz_term:api_ne_binary().
account_db(#cb_context{db_name=AcctDb}) -> AcctDb.

-spec profile_id(context()) -> kz_term:api_ne_binary().
profile_id(#cb_context{profile_id = Value}) -> Value.

-spec account_modb(context()) -> kz_term:api_ne_binary().
account_modb(Context) ->
    kazoo_modb:get_modb(account_id(Context)).

-spec account_modb(context(), kz_time:now() | timeout()) -> kz_term:api_ne_binary().
account_modb(Context, {_,_,_}=Timestamp) ->
    kazoo_modb:get_modb(account_id(Context), Timestamp);
account_modb(Context, Timestamp) when is_integer(Timestamp), Timestamp > 0 ->
    kazoo_modb:get_modb(account_id(Context), Timestamp).

-spec account_modb(context(), kz_time:year(), kz_time:month()) -> kz_term:api_ne_binary().
account_modb(Context, Year, Month) ->
    kazoo_modb:get_modb(account_id(Context), Year, Month).

-spec account_realm(context()) -> kz_term:api_ne_binary().
account_realm(Context) ->
    kz_account:realm(account_doc(Context)).

-spec account_doc(context()) -> kz_term:api_object().
account_doc(#cb_context{account_id = undefined}) -> undefined;
account_doc(#cb_context{account_id = AccountId}) ->
    case kz_account:fetch(AccountId) of
        {ok, AccountJObj} -> AccountJObj;
        {error, _R} ->
            lager:warning("error fetching account doc for ~p: ~p", [AccountId,_R]),
            undefined
    end.

-spec is_authenticated(context()) -> boolean().
is_authenticated(#cb_context{auth_doc='undefined'}) -> 'false';
is_authenticated(#cb_context{}) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(kz_term:api_ne_binary() | context()) -> boolean().
is_superduper_admin('undefined') -> 'false';
is_superduper_admin(AccountId=?NE_BINARY) ->
    lager:debug("checking for superduper admin: ~s", [AccountId]),
    case kz_util:is_system_admin(AccountId) of
        'true' ->
            lager:debug("the requestor is a superduper admin"),
            'true';
        'false' ->
            lager:debug("the requestor is not a superduper admin"),
            'false'
    end;
is_superduper_admin(Context) ->
    is_superduper_admin(auth_account_id(Context)).

-spec is_account_admin(context()) -> boolean().
is_account_admin(Context) ->
    AuthAccountId = auth_account_id(Context),
    AuthUserId = auth_user_id(Context),
    lager:debug("checking if user ~s is account admin of ~s", [AuthAccountId, AuthUserId]),
    case kzd_user:is_account_admin(AuthAccountId, AuthUserId) of
        'true' ->
            lager:debug("the requestor is an account admin"),
            'true';
        'false' ->
            lager:debug("the requestor is an superduper admin"),
            'false'
    end.

-spec auth_token_type(context()) -> 'x-auth-token' | 'basic' | 'oauth' | 'unknown'.
auth_token_type(#cb_context{auth_token_type=AuthTokenType}) -> AuthTokenType.

-spec auth_token(context()) -> kz_term:api_ne_binary().
auth_token(#cb_context{auth_token=AuthToken}) -> AuthToken.

-spec auth_doc(context()) -> kz_term:api_object().
auth_doc(#cb_context{auth_doc=AuthDoc}) -> AuthDoc.

-spec auth_account_id(context()) -> kz_term:api_ne_binary().
auth_account_id(#cb_context{auth_account_id=AuthBy}) -> AuthBy.

-spec auth_account_doc(context()) -> kz_term:api_object().
auth_account_doc(#cb_context{auth_account_id = undefined}) -> undefined;
auth_account_doc(#cb_context{auth_account_id = AccountId}) ->
    case kz_account:fetch(AccountId) of
        {ok, AuthAccountJObj} -> AuthAccountJObj;
        {error, _R} ->
            lager:warning("error fetching auth account doc for ~p: ~p", [AccountId,_R]),
            undefined
    end.

-spec auth_user_id(context()) -> kz_term:api_ne_binary().
auth_user_id(#cb_context{auth_doc='undefined'}) -> 'undefined';
auth_user_id(#cb_context{auth_doc=JObj}) ->
    case kz_doc:type(JObj) of
        <<"user">> -> kz_doc:id(JObj);
        _ -> kz_json:get_value(<<"owner_id">>, JObj)
    end.

-spec req_verb(context()) -> http_method().
req_verb(#cb_context{req_verb=ReqVerb}) -> ReqVerb.

-spec req_data(context()) -> kz_json:json_term().
req_data(#cb_context{req_data=ReqData}) -> ReqData.

-spec req_files(context()) -> req_files().
req_files(#cb_context{req_files=ReqFiles}) -> ReqFiles.

-spec req_nouns(context()) -> req_nouns().
req_nouns(#cb_context{req_nouns=ReqNouns}) -> ReqNouns.

-spec req_headers(context()) -> cowboy:http_headers().
req_headers(#cb_context{req_headers=Hs}) -> Hs.

-spec req_header(context(), binary()) -> iodata() | 'undefined'.
req_header(#cb_context{req_headers=Hs}, K) -> maps:get(K, Hs, 'undefined').

-spec query_string(context()) -> kz_json:object().
query_string(#cb_context{query_json=Q}) -> Q.

-spec req_param(context(), kz_term:ne_binary()) -> kz_json:api_json_term().
req_param(#cb_context{}=Context, K) -> req_param(Context, K, 'undefined').

-spec req_param(context(), kz_term:ne_binary(), Default) -> kz_json:json_term() | Default.
req_param(#cb_context{query_json=JObj}, K, Default) -> kz_json:get_value(K, JObj, Default).

-spec client_ip(context()) -> kz_term:api_ne_binary().
client_ip(#cb_context{client_ip=IP}) -> IP.

-spec req_id(context()) -> kz_term:ne_binary().
req_id(#cb_context{req_id=ReqId}) -> ReqId.

-spec doc(context()) -> kz_term:api_object() | kz_json:objects().
doc(#cb_context{doc=Doc}) -> Doc.

-spec load_merge_bypass(context()) -> kz_term:api_object().
load_merge_bypass(#cb_context{load_merge_bypass=ByPass}) -> ByPass.

-spec start(context()) -> kz_time:now().
start(#cb_context{start=Start}) -> Start.

-spec resp_file(context()) -> binary().
resp_file(#cb_context{resp_file=RespFile}) -> RespFile.

-spec resp_data(context()) -> resp_data().
resp_data(#cb_context{resp_data=RespData}) -> RespData.

-spec resp_status(context()) -> crossbar_status().
resp_status(#cb_context{resp_status=RespStatus}) -> RespStatus.

-spec resp_expires(context()) -> kz_time:datetime().
resp_expires(#cb_context{resp_expires=RespExpires}) -> RespExpires.

-spec resp_headers(context()) -> cowboy:http_headers().
resp_headers(#cb_context{resp_headers=RespHeaders}) -> RespHeaders.

-spec api_version(context()) -> kz_term:ne_binary().
api_version(#cb_context{api_version=ApiVersion}) -> ApiVersion.

-spec resp_etag(context()) -> 'automatic' | string() | kz_term:api_binary().
resp_etag(#cb_context{resp_etag=ETag}) -> ETag.

-spec resp_envelope(context()) -> kz_json:object().
resp_envelope(#cb_context{resp_envelope=E}) -> E.

-spec allow_methods(context()) -> http_methods().
allow_methods(#cb_context{allow_methods=AMs}) -> AMs.

-spec allowed_methods(context()) -> http_methods().
allowed_methods(#cb_context{allowed_methods=AMs}) -> AMs.

-spec method(context()) -> http_method().
method(#cb_context{method=M}) -> M.

-spec pretty_print(context()) -> boolean().
pretty_print(#cb_context{pretty_print = PrettyPrint}) -> PrettyPrint.

-spec path_token(binary()) -> binary().
path_token(Token) ->
    kz_util:uri_decode(Token).

-spec path_tokens(context()) -> kz_term:ne_binaries().
path_tokens(#cb_context{raw_path=Path}) ->
    [path_token(kz_util:uri_decode(Token))
     || Token <- binary:split(Path, <<"/">>, ['global', 'trim'])
    ].

-spec magic_pathed(context()) -> boolean().
magic_pathed(#cb_context{magic_pathed=MP}) -> MP.

-spec should_paginate(context()) -> boolean().
should_paginate(#cb_context{api_version=?VERSION_1}) ->
    lager:debug("pagination disabled in this API version"),
    'false';
should_paginate(#cb_context{should_paginate='undefined'}=Context) ->
    case req_value(Context, <<"paginate">>) of
        'undefined' -> 'true';
        ShouldPaginate ->
            lager:debug("request has paginate flag: ~s", [ShouldPaginate]),
            kz_term:is_true(ShouldPaginate)
    end;
should_paginate(#cb_context{should_paginate=Should}) -> Should.

-spec pagination_page_size() -> pos_integer().
pagination_page_size() ->
    ?PAGINATION_PAGE_SIZE.

-spec pagination_page_size(context()) -> kz_term:api_pos_integer().
pagination_page_size(Context) ->
    pagination_page_size(Context, api_version(Context)).

-spec pagination_page_size(context(), kz_term:ne_binary()) -> kz_term:api_pos_integer().
pagination_page_size(_Context, ?VERSION_1) -> 'undefined';
pagination_page_size(Context, _Version) ->
    case req_value(Context, <<"page_size">>) of
        'undefined' -> pagination_page_size();
        V ->
            case kz_term:safe_cast(V, 'undefined', fun kz_term:to_integer/1) of
                'undefined' ->
                    lager:debug("can not convert ~p to integer, using default page size", [V]),
                    pagination_page_size();
                PageSize when PageSize > 0 -> PageSize;
                _ ->
                    lager:debug("~p is not bigger than zero, using default page size", [V]),
                    pagination_page_size()
            end
    end.

-spec should_soft_delete(context()) -> boolean().
should_soft_delete(#cb_context{}=Context) ->
    kz_term:is_true(req_value(Context, <<"should_soft_delete">>, ?SOFT_DELETE)).

-spec req_json(context()) -> kz_json:object().
req_json(#cb_context{req_json=RJ}) -> RJ.

-spec content_types_accepted(context()) -> crossbar_content_handlers().
content_types_accepted(#cb_context{content_types_accepted=CTAs}) -> CTAs.

-spec content_types_provided(context()) -> crossbar_content_handlers().
content_types_provided(#cb_context{content_types_provided=CTPs}) -> CTPs.

-spec languages_provided(context()) -> kz_term:ne_binaries().
languages_provided(#cb_context{languages_provided=LP}) -> LP.

-spec encodings_provided(context()) -> kz_term:ne_binaries().
encodings_provided(#cb_context{encodings_provided=EP}) -> EP.

-spec validation_errors(context()) -> kz_json:object().
validation_errors(#cb_context{validation_errors=Errs}) -> Errs.

-spec resp_error_code(context()) -> kz_term:api_integer().
resp_error_code(#cb_context{resp_error_code=Code}) -> Code.

-spec resp_error_msg(context()) -> kz_term:api_ne_binary().
resp_error_msg(#cb_context{resp_error_msg=Msg}) -> Msg.

%% Setters
-spec setters(context(), setters()) -> context().
setters(#cb_context{}=Context, []) -> Context;
setters(#cb_context{}=Context, [_|_]=Setters) ->
    lists:foldl(fun setters_fold/2, Context, Setters).

-spec setters_fold(setter_kv(), context() | kz_json:object()) ->
                          context() | kz_json:object().
setters_fold({F, V}, C) -> F(C, V);
setters_fold({F, K, V}, C) -> F(C, K, V);
setters_fold(F, C) when is_function(F, 1) -> F(C).


-spec set_account_id(context(), kz_term:ne_binary()) -> context().
set_account_id(#cb_context{}=Context, AcctId) ->
    Context#cb_context{account_id=AcctId}.

-spec set_account_name(context(), kz_term:api_ne_binary()) -> context().
set_account_name(#cb_context{}=Context, Name) ->
    Context#cb_context{account_name=Name}.

-spec set_user_id(context(), kz_term:ne_binary()) -> context().
set_user_id(#cb_context{}=Context, UserId) ->
    Context#cb_context{user_id=UserId}.

-spec set_device_id(context(), kz_term:ne_binary()) -> context().
set_device_id(#cb_context{}=Context, DeviceId) ->
    Context#cb_context{device_id=DeviceId}.

-spec set_reseller_id(context(), kz_term:api_ne_binary()) -> context().
set_reseller_id(#cb_context{}=Context, AcctId) ->
    Context#cb_context{reseller_id=AcctId}.

-spec set_account_db(context(), kz_term:ne_binary()) -> context().
set_account_db(#cb_context{}=Context, AcctDb) ->
    Context#cb_context{db_name=AcctDb}.

-spec set_account_modb(context(), kz_time:year() | kz_term:ne_binary(), kz_time:month() | kz_term:ne_binary()) -> context().
set_account_modb(#cb_context{}=Context, Year, Month) ->
    Context#cb_context{db_name=kazoo_modb:get_modb(account_id(Context), Year, Month)}.

-spec set_account_modb(context(), kz_term:ne_binary(), kz_time:year() | kz_term:ne_binary(), kz_time:month() | kz_term:ne_binary()) -> context().
set_account_modb(#cb_context{}=Context, AcctId, Year, Month) ->
    Context#cb_context{db_name=kazoo_modb:get_modb(AcctId, Year, Month)}.

-spec set_auth_token_type(context(), 'x-auth-token' | 'basic' | 'oauth' | 'unknown') -> context().
set_auth_token_type(#cb_context{}=Context, AuthTokenType) ->
    Context#cb_context{auth_token_type=AuthTokenType}.

-spec set_auth_token(context(), kz_term:ne_binary()) -> context().
set_auth_token(#cb_context{}=Context, AuthToken) ->
    Context#cb_context{auth_token=AuthToken}.

-spec set_auth_doc(context(), kz_json:object()) -> context().
set_auth_doc(#cb_context{}=Context, AuthDoc) ->
    Context#cb_context{auth_doc=AuthDoc}.

-spec set_auth_account_id(context(), kz_term:ne_binary()) -> context().
set_auth_account_id(#cb_context{}=Context, AuthBy) ->
    Context#cb_context{auth_account_id=AuthBy}.

-spec set_req_verb(context(), http_method()) -> context().
set_req_verb(#cb_context{}=Context, ReqVerb) ->
    Context#cb_context{req_verb=ReqVerb}.

-spec set_req_data(context(), kz_json:json_term()) -> context().
set_req_data(#cb_context{}=Context, ReqData) ->
    Context#cb_context{req_data=ReqData}.

-spec set_req_files(context(), req_files()) -> context().
set_req_files(#cb_context{}=Context, ReqFiles) ->
    Context#cb_context{req_files=ReqFiles}.

-spec set_req_nouns(context(), req_nouns()) -> context().
set_req_nouns(#cb_context{}=Context, ReqNouns) ->
    Context#cb_context{req_nouns=ReqNouns}.

-spec set_req_headers(context(), cowboy:http_headers()) -> context().
set_req_headers(#cb_context{}=Context, ReqHs) ->
    Context#cb_context{req_headers=ReqHs}.

-spec set_req_header(context(), kz_term:ne_binary(), iodata()) -> context().
set_req_header(#cb_context{req_headers=ReqHs}=Context, HKey, HValue) ->
    Context#cb_context{req_headers=maps:put(HKey, HValue, ReqHs)}.

-spec set_query_string(context(), kz_json:object()) -> context().
set_query_string(#cb_context{}=Context, Q) ->
    Context#cb_context{query_json=Q}.

-spec set_req_id(context(), kz_term:ne_binary()) -> context().
set_req_id(#cb_context{}=Context, ReqId) ->
    Context#cb_context{req_id=ReqId}.

-spec set_doc(context(), kz_json:api_json_term() | kz_json:objects()) -> context().
set_doc(#cb_context{}=Context, Doc) ->
    Context#cb_context{doc=Doc}.

-spec set_load_merge_bypass(context(), kz_term:api_ne_binary()) -> context().
set_load_merge_bypass(#cb_context{}=Context, JObj) ->
    Context#cb_context{load_merge_bypass=JObj}.

-spec set_start(context(), kz_time:now()) -> context().
set_start(#cb_context{}=Context, Start) ->
    Context#cb_context{start=Start}.

-spec set_resp_file(context(), kz_term:api_binary()) -> context().
set_resp_file(#cb_context{}=Context, RespFile) ->
    Context#cb_context{resp_file=RespFile}.

-spec set_resp_data(context(), resp_data()) -> context().
set_resp_data(#cb_context{}=Context, RespData) ->
    Context#cb_context{resp_data=RespData}.

-spec set_resp_status(context(), crossbar_status()) -> context().
set_resp_status(#cb_context{}=Context, RespStatus) ->
    Context#cb_context{resp_status=RespStatus}.

-spec set_resp_expires(context(), kz_time:datetime()) -> context().
set_resp_expires(#cb_context{}=Context, RespExpires) ->
    Context#cb_context{resp_expires=RespExpires}.

-spec set_api_version(context(), kz_term:ne_binary() | pos_integer()) -> context().
set_api_version(#cb_context{}=Context, ApiVersion) ->
    Context#cb_context{api_version=kz_term:to_binary(ApiVersion)}.

-spec set_resp_etag(context(), kz_term:api_binary()) -> context().
set_resp_etag(#cb_context{}=Context, ETag) ->
    Context#cb_context{resp_etag=ETag}.

-spec set_resp_envelope(context(), kz_json:object()) -> context().
set_resp_envelope(#cb_context{}=Context, E) ->
    Context#cb_context{resp_envelope=E}.

-spec set_allow_methods(context(), http_methods()) -> context().
set_allow_methods(#cb_context{}=Context, AMs) ->
    Context#cb_context{allow_methods=AMs}.

-spec set_allowed_methods(context(), http_methods()) -> context().
set_allowed_methods(#cb_context{}=Context, AMs) ->
    Context#cb_context{allowed_methods=AMs}.

-spec set_method(context(), http_method()) -> context().
set_method(#cb_context{}=Context, M) ->
    Context#cb_context{method=M}.

-spec set_req_json(context(), kz_json:object()) -> context().
set_req_json(#cb_context{}=Context, RJ) ->
    Context#cb_context{req_json=RJ}.

-spec set_content_types_accepted(context(), crossbar_content_handlers()) -> context().
set_content_types_accepted(#cb_context{}=Context, CTAs) ->
    Context#cb_context{content_types_accepted=CTAs}.

-spec set_content_types_provided(context(), crossbar_content_handlers()) -> context().
set_content_types_provided(#cb_context{}=Context, CTPs) ->
    Context#cb_context{content_types_provided=CTPs}.

-spec set_languages_provided(context(), kz_term:ne_binaries()) -> context().
set_languages_provided(#cb_context{}=Context, LP) ->
    Context#cb_context{languages_provided=LP}.

-spec set_encodings_provided(context(), kz_term:ne_binaries()) -> context().
set_encodings_provided(#cb_context{}=Context, EP) ->
    Context#cb_context{encodings_provided=EP}.

-spec set_magic_pathed(context(), boolean()) -> context().
set_magic_pathed(#cb_context{}=Context, MP) ->
    Context#cb_context{magic_pathed=kz_term:is_true(MP)}.

-spec set_should_paginate(context(), boolean()) -> context().
set_should_paginate(#cb_context{}=Context, SP) ->
    Context#cb_context{should_paginate=kz_term:is_true(SP)}.

-spec set_resp_error_code(context(), integer()) -> context().
set_resp_error_code(#cb_context{}=Context, Code) ->
    Context#cb_context{resp_error_code=Code}.

-spec set_resp_error_msg(context(), kz_term:api_ne_binary()) -> context().
set_resp_error_msg(#cb_context{}=Context, Msg) ->
    Context#cb_context{resp_error_msg=Msg}.

-spec set_resp_headers(context(), cowboy:http_headers()) -> context().
set_resp_headers(#cb_context{resp_headers=Hs}=Context, Headers) ->
    Context#cb_context{resp_headers=maps:merge(Hs, Headers)}.

-spec set_resp_header(context(), kz_term:ne_binary(), kz_term:ne_binary()) -> context().
set_resp_header(#cb_context{resp_headers=RespHeaders}=Context, K, V) ->
    Context#cb_context{resp_headers=maps:put(K, V, RespHeaders)}.

-spec add_resp_headers(context(), cowboy:http_headers()) -> context().
add_resp_headers(#cb_context{resp_headers=RespHeaders}=Context, #{}=Headers) ->
    Context#cb_context{resp_headers=maps:fold(fun add_resp_header_fold/3, RespHeaders, Headers)}.

add_resp_header_fold(K, V, RHs) ->
    maps:put(kz_term:to_lower_binary(K), V, RHs).

-spec add_resp_header(context(), kz_term:ne_binary(), kz_term:ne_binary()) -> context().
add_resp_header(#cb_context{resp_headers=RespHeaders}=Context, K, V) ->
    Context#cb_context{resp_headers=add_resp_header_fold(K, V, RespHeaders)}.

-spec set_validation_errors(context(), kz_json:object()) -> context().
set_validation_errors(#cb_context{}=Context, Errors) ->
    Context#cb_context{validation_errors=Errors}.

-spec set_port(context(), integer()) -> context().
set_port(#cb_context{}=Context, Value) ->
    Context#cb_context{port = Value}.
-spec set_host_url(context(), binary()) -> context().
set_host_url(#cb_context{}=Context, Value) ->
    Context#cb_context{host_url = Value}.

-spec host_url(context()) -> binary().
host_url(#cb_context{host_url = Value}) -> Value.

-spec set_pretty_print(context(), boolean()) -> context().
set_pretty_print(#cb_context{}=Context, Value) ->
    Context#cb_context{pretty_print = Value}.

-spec set_raw_path(context(), binary()) -> context().
set_raw_path(#cb_context{}=Context, Value) ->
    Context#cb_context{raw_path = Value}.

-spec raw_qs(context()) -> kz_term:api_ne_binary().
raw_qs(#cb_context{raw_qs=QS}) ->
    QS.

-spec set_raw_qs(context(), binary()) -> context().
set_raw_qs(#cb_context{}=Context, Value) ->
    Context#cb_context{raw_qs = Value}.

-spec set_client_ip(context(), kz_term:ne_binary()) -> context().
set_client_ip(#cb_context{}=Context, Value) ->
    Context#cb_context{client_ip = Value}.

-spec set_profile_id(context(), kz_term:ne_binary()) -> context().
set_profile_id(#cb_context{}=Context, Value) ->
    Context#cb_context{profile_id = Value}.


-spec update_doc(context(), setter_kv() | setters()) -> context().
update_doc(#cb_context{doc=Doc}=Context, Updaters)
  when is_list(Updaters) ->
    Context#cb_context{doc=lists:foldl(fun setters_fold/2, Doc, Updaters)};
update_doc(#cb_context{doc=Doc}=Context, Updater) ->
    Context#cb_context{doc=setters_fold(Updater, Doc)}.

%% Helpers

-spec add_content_types_provided(context(), crossbar_content_handler() | crossbar_content_handlers()) ->
                                        context().
add_content_types_provided(#cb_context{content_types_provided=CTPs}=Context, [_|_]=NewCTPs) ->
    Context#cb_context{content_types_provided = NewCTPs ++ CTPs};
add_content_types_provided(#cb_context{}=Context, {_, _}=NewCTP) ->
    add_content_types_provided(Context,[NewCTP]).

-spec add_content_types_accepted(context(), crossbar_content_handler() | crossbar_content_handlers()) ->
                                        context().
add_content_types_accepted(#cb_context{content_types_accepted=CTAs}=Context, [_|_]=NewCTAs) ->
    Context#cb_context{content_types_accepted = NewCTAs ++ CTAs};
add_content_types_accepted(#cb_context{}=Context, {_, _}=NewCTA) ->
    add_content_types_provided(Context,[NewCTA]).

-spec add_attachment_content_type(context(), kz_term:ne_binary(), kz_term:ne_binary()) -> context().
add_attachment_content_type(#cb_context{}=Context, DocId, AttachmentId) ->
    Context1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION_ANY),
    case resp_status(Context1) of
        'success' ->
            maybe_add_content_type_provided(Context1, AttachmentId);
        _Status -> Context1
    end.

-spec maybe_add_content_type_provided(context(), kz_term:ne_binary()) -> context().
maybe_add_content_type_provided(Context, AttachmentId) ->
    case kz_doc:attachment_content_type(doc(Context), AttachmentId) of
        'undefined' -> Context;
        ContentType ->
            lager:debug("found content type ~s", [ContentType]),
            [Type, SubType] = binary:split(ContentType, <<"/">>),
            add_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store(context(), any(), any()) -> context().
store(#cb_context{storage=Storage}=Context, Key, Data) ->
    Context#cb_context{storage=[{Key, Data} | props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------

-spec fetch(context(), any()) -> any().
fetch(#cb_context{}=Context, Key) ->
    fetch(Context, Key, 'undefined').

-spec fetch(context(), any(), any()) -> any().
fetch(#cb_context{storage=Storage}, Key, Default) ->
    case props:get_value(Key, Storage) of
        'undefined' -> Default;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid(context()) -> 'ok'.
put_reqid(#cb_context{req_id=ReqId}) ->
    kz_util:put_callid(ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec has_errors(context()) -> boolean().
has_errors(#cb_context{validation_errors=JObj
                      ,resp_status='success'
                      }) ->
    not kz_term:is_empty(JObj);
has_errors(#cb_context{}) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec import_errors(context()) -> context().
import_errors(#cb_context{}=Context) ->
    case response(Context) of
        {'ok', _} -> Context;
        {'error', {ErrorCode, ErrorMsg, Errors}} ->
            Context#cb_context{resp_error_code=ErrorCode
                              ,resp_error_msg=ErrorMsg
                              ,resp_data=Errors
                              ,resp_status='error'
                              }
    end.

-spec response(context()) -> {ok, kz_json:object()} |
                             {error, {pos_integer(), kz_term:ne_binary(), kz_json:object()}}.
response(#cb_context{resp_status='success'
                    ,resp_data=JObj
                    }) ->
    {'ok', JObj};
response(#cb_context{resp_error_code=Code
                    ,resp_error_msg=Msg
                    ,resp_data=DataJObj
                    ,validation_errors=ValidationJObj
                    }) ->
    ErrorCode = try kz_term:to_integer(Code) catch _:_ -> 500 end,
    ErrorMsg = case kz_term:is_empty(Msg) of
                   'false' -> kz_term:to_binary(Msg);
                   'true' -> <<"generic_error">>
               end,
    ErrorData = case {kz_term:is_empty(ValidationJObj), kz_term:is_empty(DataJObj)} of
                    {'false', _} -> ValidationJObj;
                    {_, _} -> DataJObj
                end,
    {'error', {ErrorCode, ErrorMsg, ErrorData}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
-type after_fun() :: fun((context()) -> context()) | 'undefined'.

-spec validate_request_data(kz_term:ne_binary() | kz_term:api_object(), context()) ->
                                   context().
validate_request_data(SchemaId, Context) ->
    validate_request_data(SchemaId, Context, 'undefined').

-spec validate_request_data(kz_term:ne_binary() | kz_term:api_object(), context(), after_fun()) ->
                                   context().
validate_request_data(SchemaId, Context, OnSuccess) ->
    validate_request_data(SchemaId, Context, OnSuccess, 'undefined').

-spec validate_request_data(kz_term:ne_binary() | kz_term:api_object(), context(), after_fun(), after_fun()) ->
                                   context().
validate_request_data(SchemaId, Context, OnSuccess, OnFailure) ->
    SchemaRequired = fetch(Context, 'ensure_valid_schema', ?SHOULD_ENSURE_SCHEMA_IS_VALID),
    validate_request_data(SchemaId, Context, OnSuccess, OnFailure, SchemaRequired).

-spec validate_request_data(kz_term:ne_binary() | kz_term:api_object(), context(), after_fun(), after_fun(), boolean()) ->
                                   context().
validate_request_data('undefined', Context, OnSuccess, _OnFailure, 'false') ->
    lager:error("schema id or schema JSON not defined, continuing anyway"),
    validate_passed(Context, OnSuccess);
validate_request_data('undefined', Context, _OnSuccess, _OnFailure, 'true') ->
    Msg = <<"schema id or schema JSON not defined.">>,
    lager:error("~s", [Msg]),
    system_error(Context, Msg);
validate_request_data(?NE_BINARY=SchemaId, Context, OnSuccess, OnFailure, SchemaRequired) ->
    case find_schema(SchemaId) of
        'undefined' when SchemaRequired ->
            lager:error("schema ~s not found, and is required", [SchemaId]),
            system_error(Context, <<"schema ", SchemaId/binary, " not found.">>);
        'undefined' ->
            lager:error("schema ~s not found, continuing anyway", [SchemaId]),
            validate_passed(Context, OnSuccess);
        SchemaJObj ->
            validate_request_data(SchemaJObj, Context, OnSuccess, OnFailure, SchemaRequired)
    end;
validate_request_data(SchemaJObj, Context, OnSuccess, OnFailure, _SchemaRequired) ->
    Strict = fetch(Context, 'schema_strict_validation', ?SHOULD_FAIL_ON_INVALID_DATA),
    try kz_json_schema:validate(SchemaJObj, kz_doc:public_fields(req_data(Context))) of
        {'ok', JObj} ->
            lager:debug("validation passed"),
            validate_passed(set_req_data(Context, JObj), OnSuccess);
        {'error', Errors} when Strict ->
            lager:debug("validation errors when strictly validating"),
            validate_failed(SchemaJObj, Context, Errors, OnFailure);
        {'error', Errors} ->
            lager:debug("validation errors but not stricly validating, trying to fix request"),
            maybe_fix_js_types(SchemaJObj, Context, OnSuccess, OnFailure, Errors)
    catch
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            lager:debug("function clause failure"),
            kz_util:log_stacktrace(ST),
            Context#cb_context{resp_status = 'fatal'
                              ,resp_error_code = 500
                              ,resp_data = kz_json:new()
                              ,resp_error_msg = <<"validation failed to run on the server">>
                              }
    end.

-spec validate_failed(kz_json:object(), context(), validation_errors(), after_fun()) -> context().
validate_failed(SchemaJObj, Context, Errors, OnFailure) ->
    lager:debug("validation failed ~s: ~p", [kz_doc:id(SchemaJObj), Errors]),
    Context1 = failed(Context, Errors),
    case is_function(OnFailure, 1) of
        'true' -> OnFailure(Context1);
        'false' -> Context1
    end.

-spec validate_passed(context(), after_fun()) -> context().
validate_passed(Context, OnSuccess) ->
    Context1 = passed(copy_req_data_to_doc(Context)),
    case is_function(OnSuccess, 1) of
        'true' -> OnSuccess(Context1);
        'false' -> Context1
    end.

-spec copy_req_data_to_doc(context()) -> context().
copy_req_data_to_doc(Context) ->
    NewDoc = case doc(Context) of
                 'undefined' -> req_data(Context);
                 Doc -> kz_json:merge_jobjs(kz_doc:private_fields(Doc), req_data(Context))
             end,
    set_doc(Context, NewDoc).

-spec failed(context(), validation_errors()) -> context().
failed(Context, Errors) ->
    Context1 = setters(Context, [{fun set_resp_error_code/2, 400}
                                ,{fun set_resp_status/2, 'error'}
                                ,{fun set_resp_error_msg/2, <<"validation failed">>}
                                ]),
    lists:foldl(fun failed_error/2, Context1, Errors).

-spec failed_error(validation_error(), context()) -> context().
failed_error(Error, Context) ->
    Props = props:filter_undefined([{'version', api_version(Context)}
                                   ,{'error_code', resp_error_code(Context)}
                                   ,{'error_message', resp_error_msg(Context)}
                                   ]),
    {ErrorCode, ErrorMessage, ErrorJObj} = kz_json_schema:error_to_jobj(Error, Props),
    JObj = validation_errors(Context),
    Context#cb_context{validation_errors = kz_json:merge_jobjs(ErrorJObj, JObj)
                      ,resp_error_code = ErrorCode
                      ,resp_data = kz_json:new()
                      ,resp_error_msg = ErrorMessage
                      }.

-spec passed(context()) -> context().
passed(Context) ->
    Context1 = case error =:= resp_status(Context) of
                   true -> Context;
                   false -> set_resp_status(Context, success)
               end,
    case kz_doc:id(req_data(Context1)) of
        'undefined' -> Context1;
        Id -> set_doc(Context1, kz_doc:set_id(doc(Context1), Id))
    end.

-spec find_schema(kz_term:ne_binary()) -> kz_term:api_object().
find_schema(Schema=?NE_BINARY) ->
    case kz_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:error("failed to find schema ~s: ~p", [Schema, _E]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec add_system_error(atom() | binary(), context()) -> context().
add_system_error('too_many_requests', Context) ->
    build_system_error(429, 'too_many_requests', <<"too many requests">>, Context);
add_system_error('no_credit', Context) ->
    build_system_error(402, 'no_credit', <<"not enough credit to perform action">>, Context);
add_system_error('unspecified_fault', Context) ->
    build_system_error(500, 'unspecified_fault', <<"unspecified fault">>, Context);
add_system_error('account_cant_create_tree', Context) ->
    build_system_error(500, 'account_cant_create_tree', <<"account creation fault">>, Context);
add_system_error('account_has_descendants', Context) ->
    build_system_error(500, 'account_has_descendants', <<"account has descendants">>, Context);
add_system_error('faulty_request', Context) ->
    build_system_error(404, 'faulty_request', <<"faulty request">>, Context);

add_system_error('bad_identifier', Context) ->
    build_system_error(404, 'bad_identifier', <<"bad identifier">>, Context);

add_system_error('forbidden', Context) ->
    build_system_error(403, 'forbidden', <<"forbidden">>, Context);
add_system_error('invalid_credentials', Context) ->
    build_system_error(401, 'invalid_credentials', <<"invalid credentials">>, Context);

add_system_error('datastore_missing', Context) ->
    build_system_error(503, 'datastore_missing', <<"data collection missing: database not found">>, Context);
add_system_error('datastore_missing_view', Context) ->
    build_system_error(503, 'datastore_missing_view', <<"datastore missing view">>, Context);
add_system_error('datastore_conflict', Context) ->
    build_system_error(409, 'datastore_conflict', <<"conflicting documents">>, Context);
add_system_error('datastore_unreachable', Context) ->
    build_system_error(503, 'datastore_unreachable', <<"datastore timeout">>, Context);
add_system_error('datastore_fault', Context) ->
    build_system_error(503, 'datastore_fault', <<"datastore fatal error">>, Context);
add_system_error('empty_tree_accounts_exist', Context) ->
    build_system_error(400, 'empty_tree_accounts_exist', <<"unable to create account tree">>, Context);

add_system_error('parse_error', Context) ->
    build_system_error(400, 'parse_error', <<"failed to parse request body">>, Context);
add_system_error('invalid_method', Context) ->
    build_system_error(405, 'invalid_method', <<"method not allowed">>, Context);
add_system_error('not_found', Context) ->
    build_system_error(404, 'not_found', <<"not found">>, Context);
add_system_error('disabled', Context) ->
    build_system_error(400, 'disabled', <<"entity disabled">>, Context);
add_system_error(Error, Context) ->
    build_system_error(500, Error, kz_term:to_binary(Error), Context).

-spec add_system_error(atom() | binary(), kz_term:ne_binary() | kz_json:object(), context()) -> context().
add_system_error(Error, <<_/binary>>=Message, Context) ->
    JObj = kz_json:from_list([{<<"message">>, Message}]),
    add_system_error(Error, JObj, Context);
add_system_error(Error, Props, Context) when is_list(Props) ->
    JObj = kz_json:from_list(Props),
    add_system_error(Error, JObj, Context);
add_system_error('bad_identifier'=Error, JObj, Context) ->
    J = kz_json:set_value(<<"message">>, <<"bad identifier">>, JObj),
    build_system_error(404, Error, J, Context);
add_system_error('not_found', JObj, Context) ->
    add_system_error('bad_identifier', JObj, Context);
add_system_error('invalid_bulk_type'=Error, JObj, Context) ->
    %% TODO: JObj is expected to have a type key!!
    Type = kz_json:get_value(<<"type">>, JObj),
    Message = <<"bulk operations do not support documents of type ", (kz_term:to_binary(Type))/binary>>,
    J = kz_json:set_value(<<"message">>, Message, JObj),
    build_system_error(400, Error, J, Context);
add_system_error('forbidden'=Error, JObj, Context) ->
    J = kz_json:set_value(<<"message">>, <<"forbidden">>, JObj),
    build_system_error(403, Error, J, Context);
add_system_error('timeout'=Error, JObj, Context) ->
    J = kz_json:set_value(<<"message">>, <<"timeout">>, JObj),
    build_system_error(500, Error, J, Context);
add_system_error('invalid_method'=Error, JObj, Context) ->
    J = kz_json:set_value(<<"message">>, <<"invalid method">>, JObj),
    build_system_error(405, Error, J, Context);
add_system_error('bad_gateway'=Error, JObj, Context) ->
    build_system_error(502, Error, JObj, Context);
add_system_error('multiple_choice'=Error, JObj, Context) ->
    build_system_error(400, Error, JObj, Context);
add_system_error(Error, JObj, Context) ->
    case kz_json:get_ne_value(<<"message">>, JObj) of
        'undefined' ->
            J = kz_json:set_value(<<"message">>, <<"unknown failure">>, JObj),
            build_system_error(500, Error, J, Context);
        _Else ->
            build_system_error(500, Error, JObj, Context)
    end.

-spec add_system_error(integer(), atom() | kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), context()) -> context().
add_system_error(Code, Error, JObj, Context) ->
    build_system_error(Code, Error, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_system_error(integer(), atom() | kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), context()) -> context().
build_system_error(Code, Error, <<_/binary>> = Message, Context) ->
    build_system_error(Code, Error, kz_json:from_list([{<<"message">>, Message}]), Context);
build_system_error(Code, Error, JObj, Context) ->
    ApiVersion = api_version(Context),
    Message = kz_json_schema:build_error_message(ApiVersion, JObj),
    Context#cb_context{resp_status='error'
                      ,resp_error_code=Code
                      ,resp_data=Message
                      ,resp_error_msg=kz_term:to_binary(Error)
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
-spec add_validation_error(kz_json:path(), kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object(), context()) ->
                                  context().
add_validation_error(<<_/binary>> = Property, Code, Message, Context) ->
    add_validation_error([Property], Code, Message, Context);
add_validation_error(Property, Code, <<_/binary>> = Message, Context) ->
    add_validation_error(Property, Code, kz_json:from_list([{<<"message">>, Message}]), Context);
add_validation_error(Property, Code, Message, Context) ->
    {ErrorCode, ErrorMessage, ErrorJObj} =
        kz_json_schema:validation_error(Property, Code, Message
                                       ,props:filter_undefined(
                                          [{'version', api_version(Context)}
                                          ,{'error_code', resp_error_code(Context)}
                                          ,{'error_message', resp_error_msg(Context)}
                                          ]
                                         )
                                       ),
    ErrorsJObj = validation_errors(Context),

    Context#cb_context{validation_errors=kz_json:merge_jobjs(ErrorJObj, ErrorsJObj)
                      ,resp_status='error'
                      ,resp_error_code=ErrorCode
                      ,resp_data=kz_json:new()
                      ,resp_error_msg=maybe_update_error_message(resp_error_msg(Context), ErrorMessage)
                      }.

-spec maybe_update_error_message(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_update_error_message(_Old, <<"init failed">>) -> <<"validation error">>;
maybe_update_error_message(Msg, Msg) -> Msg;
maybe_update_error_message(_Old, New) -> New.

-spec maybe_fix_js_types(kz_json:object(), context(), after_fun(), after_fun(), validation_errors()) -> context().
maybe_fix_js_types(SchemaJObj, Context, OnSuccess, OnFailure, Errors) ->
    JObj = req_data(Context),
    case lists:foldl(fun maybe_fix_js_type/2, JObj, Errors) of
        JObj -> validate_failed(SchemaJObj, Context, Errors, OnFailure);
        NewJObj ->
            validate_request_data(SchemaJObj, set_req_data(Context, NewJObj), OnSuccess, OnFailure)
    end.

-spec maybe_fix_js_type(validation_error(), kz_json:object()) -> kz_json:object().
maybe_fix_js_type({'data_invalid', SchemaJObj, 'wrong_type', Value, Key}, JObj) ->
    case kz_json:get_value(<<"type">>, SchemaJObj) of
        <<"integer">> -> maybe_fix_js_integer(Key, Value, JObj);
        <<"boolean">> -> maybe_fix_js_boolean(Key, Value, JObj);
        _Type -> JObj
    end;
maybe_fix_js_type(_, JObj) -> JObj.

-spec maybe_fix_js_integer(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
                                  kz_json:object().
maybe_fix_js_integer(Key, Value, JObj) ->
    try kz_term:to_integer(Value) of
        V -> kz_json:set_value(maybe_fix_index(Key), V, JObj)
    catch
        _E:_R ->
            lager:debug("error converting ~p to integer ~p: ~p", [Value, _E, _R]),
            JObj
    end.

-spec maybe_fix_js_boolean(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
                                  kz_json:object().
maybe_fix_js_boolean(Key, Value, JObj) ->
    try kz_term:to_boolean(Value) of
        V -> kz_json:set_value(maybe_fix_index(Key), V, JObj)
    catch
        _E:_R ->
            lager:debug("error converting ~p to boolean ~p: ~p", [Value, _E, _R]),
            JObj
    end.

-spec maybe_fix_index(kz_json:path() | kz_json:path()) -> kz_json:path() | kz_json:path().
maybe_fix_index(Keys)
  when is_list(Keys) ->
    [case is_integer(K) of
         true -> K + 1;
         false -> K
     end
     || K <- Keys
    ];
maybe_fix_index(Key) ->
    Key.

-spec system_properties(context()) -> kz_json:object().
system_properties(Context) ->
    kz_json:from_list(
      [{Key, Fun(Context)}
       || {Fun, Key} <- [{fun req_id/1, <<"req_id">>}
                        ,{fun query_string/1, <<"query_json">>}
                        ,{fun req_data/1, <<"req_data">>}
                        ,{fun req_json/1, <<"req_json">>}
                        ,{fun(C) -> kz_json:from_map(req_headers(C)) end, <<"req_headers">>}
                        ,{fun auth_account_id/1, <<"auth_account_id">>}
                        ,{fun account_name/1, <<"account_name">>}
                        ,{fun account_id/1, <<"account_id">>}
                        ]
      ]).

-spec system_error(context(), kz_term:ne_binary()) -> context().
system_error(Context, Error) ->
    Notify = props:filter_undefined(
               [{<<"Subject">>, <<"System Alert: API Error - ", Error/binary>>}
               ,{<<"Message">>, Error}
               ,{<<"Details">>, system_properties(Context)}
               ,{<<"Account-ID">>, auth_account_id(Context)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1),
    add_system_error(Error, Context).
