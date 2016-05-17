%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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
         ,req_header/2
         ,query_string/1, set_query_string/2
         ,client_ip/1, set_client_ip/2
         ,doc/1, set_doc/2, update_doc/2
         ,load_merge_bypass/1, set_load_merge_bypass/2
         ,start/1, set_start/2
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
         ,should_paginate/1, set_should_paginate/2

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

         ,set_raw_host/2
         ,set_port/2
         ,set_raw_path/2
         ,set_raw_qs/2
         ,profile_id/1 ,set_profile_id/2

         %% Special accessors
         ,req_value/2, req_value/3
         ,accepting_charges/1, set_accepting_charges/1
         ,req_param/2, req_param/3
        ]).

-include("crossbar.hrl").

-define(KEY_ACCEPT_CHARGES, <<"accept_charges">>).

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

-spec req_value(context(), kz_json:keys()) -> kz_json:json_term().
-spec req_value(context(), kz_json:keys(), Default) -> kz_json:json_term() | Default.
req_value(#cb_context{}=Context, Key) ->
    req_value(Context, Key, 'undefined').
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
    kz_util:is_true(req_value(Context, ?KEY_ACCEPT_CHARGES, 'false')).

-spec set_accepting_charges(context()) -> context().
set_accepting_charges(#cb_context{req_json = ReqJObj} = Context) ->
    NewReqJObj = kz_json:set_value(?KEY_ACCEPT_CHARGES, 'true', ReqJObj),
    set_req_json(Context, NewReqJObj).

%% Accessors
-spec account_id(context()) -> api_binary().
-spec account_db(context()) -> api_binary().
-spec user_id(context()) -> api_binary().
-spec device_id(context()) -> api_binary().
-spec account_modb(context()) -> api_binary().
-spec account_modb(context(), kz_now() | kz_timeout()) -> api_binary().
-spec account_modb(context(), kz_year(), kz_month()) -> api_binary().
-spec account_realm(context()) -> api_binary().
-spec account_doc(context()) -> api_object().
-spec profile_id(context()) -> api_binary().

account_id(#cb_context{account_id=AcctId}) -> AcctId.
account_name(#cb_context{account_name=Name}) -> Name.
user_id(#cb_context{user_id=UserId}) -> UserId.
device_id(#cb_context{device_id=DeviceId}) -> DeviceId.
reseller_id(#cb_context{reseller_id=AcctId}) -> AcctId.
account_db(#cb_context{db_name=AcctDb}) -> AcctDb.
profile_id(#cb_context{profile_id = Value}) -> Value.

account_modb(Context) ->
    kazoo_modb:get_modb(account_id(Context)).
account_modb(Context, {_,_,_}=Timestamp) ->
    kazoo_modb:get_modb(account_id(Context), Timestamp);
account_modb(Context, Timestamp) when is_integer(Timestamp), Timestamp > 0 ->
    kazoo_modb:get_modb(account_id(Context), Timestamp).
account_modb(Context, Year, Month) ->
    kazoo_modb:get_modb(account_id(Context), Year, Month).

account_realm(Context) ->
    kz_account:realm(account_doc(Context)).

account_doc(#cb_context{account_id='undefined'}) -> 'undefined';
account_doc(Context) ->
    crossbar_util:get_account_doc(account_id(Context)).

is_authenticated(#cb_context{auth_doc='undefined'}) -> 'false';
is_authenticated(#cb_context{}) -> 'true'.

auth_token_type(#cb_context{auth_token_type=AuthTokenType}) -> AuthTokenType.
auth_token(#cb_context{auth_token=AuthToken}) -> AuthToken.
auth_doc(#cb_context{auth_doc=AuthDoc}) -> AuthDoc.
auth_account_id(#cb_context{auth_account_id=AuthBy}) -> AuthBy.
auth_account_doc(Context) ->
    case auth_account_id(Context) of
        'undefined' -> 'undefined';
        AccountId ->
            crossbar_util:get_account_doc(AccountId)
    end.

auth_user_id(#cb_context{auth_doc='undefined'}) -> 'undefined';
auth_user_id(#cb_context{auth_doc=JObj}) -> kz_json:get_value(<<"owner_id">>, JObj).
req_verb(#cb_context{req_verb=ReqVerb}) -> ReqVerb.
req_data(#cb_context{req_data=ReqData}) -> ReqData.
req_files(#cb_context{req_files=ReqFiles}) -> ReqFiles.
req_nouns(#cb_context{req_nouns=ReqNouns}) -> ReqNouns.
req_headers(#cb_context{req_headers=Hs}) -> Hs.
req_header(#cb_context{req_headers=Hs}, K) -> props:get_value(K, Hs).
query_string(#cb_context{query_json=Q}) -> Q.
req_param(#cb_context{}=Context, K) -> req_param(Context, K, 'undefined').
req_param(#cb_context{query_json=JObj}, K, Default) -> kz_json:get_value(K, JObj, Default).
client_ip(#cb_context{client_ip=IP}) -> IP.
req_id(#cb_context{req_id=ReqId}) -> ReqId.
doc(#cb_context{doc=Doc}) -> Doc.
load_merge_bypass(#cb_context{load_merge_bypass=ByPass}) -> ByPass.
start(#cb_context{start=Start}) -> Start.
resp_data(#cb_context{resp_data=RespData}) -> RespData.
resp_status(#cb_context{resp_status=RespStatus}) -> RespStatus.
resp_expires(#cb_context{resp_expires=RespExpires}) -> RespExpires.
resp_headers(#cb_context{resp_headers=RespHeaders}) -> RespHeaders.
api_version(#cb_context{api_version=ApiVersion}) -> ApiVersion.
resp_etag(#cb_context{resp_etag=ETag}) -> ETag.
resp_envelope(#cb_context{resp_envelope=E}) -> E.

allow_methods(#cb_context{allow_methods=AMs}) -> AMs.
allowed_methods(#cb_context{allowed_methods=AMs}) -> AMs.
method(#cb_context{method=M}) -> M.

-spec path_tokens(context()) -> ne_binaries().
path_tokens(#cb_context{raw_path=Path}) ->
    [cow_qs:urldecode(Token) || Token <- binary:split(Path, <<"/">>, ['global', 'trim'])].

-spec magic_pathed(context()) -> boolean().
magic_pathed(#cb_context{magic_pathed=MP}) -> MP.

-spec should_paginate(context()) -> boolean().
should_paginate(#cb_context{api_version=?VERSION_1}) ->
    lager:debug("pagination disabled in this API version"),
    'false';
should_paginate(#cb_context{should_paginate='undefined'}=Context) ->
    case req_value(Context, <<"paginate">>) of
        'undefined' ->
            lager:debug("checking if request has query-string filter"),
            not crossbar_doc:has_qs_filter(Context);
        ShouldPaginate ->
            lager:debug("request has paginate flag: ~s", [ShouldPaginate]),
            kz_util:is_true(ShouldPaginate)
    end;
should_paginate(#cb_context{should_paginate=Should}) -> Should.

req_json(#cb_context{req_json=RJ}) -> RJ.
content_types_accepted(#cb_context{content_types_accepted=CTAs}) -> CTAs.
content_types_provided(#cb_context{content_types_provided=CTPs}) -> CTPs.
languages_provided(#cb_context{languages_provided=LP}) -> LP.
encodings_provided(#cb_context{encodings_provided=EP}) -> EP.

validation_errors(#cb_context{validation_errors=Errs}) -> Errs.

resp_error_code(#cb_context{resp_error_code=Code}) -> Code.
resp_error_msg(#cb_context{resp_error_msg=Msg}) -> Msg.

%% Setters
-spec setters(context(), setters()) -> context().
setters(#cb_context{}=Context, []) -> Context;
setters(#cb_context{}=Context, [_|_]=Setters) ->
    lists:foldl(fun setters_fold/2, Context, Setters).

-spec setters_fold(setter_kv(), context()) -> context().
setters_fold({F, V}, C) -> F(C, V);
setters_fold({F, K, V}, C) -> F(C, K, V);
setters_fold(F, C) when is_function(F, 1) -> F(C).

-spec set_account_id(context(), ne_binary()) -> context().
-spec set_account_db(context(), ne_binary()) -> context().
-spec set_user_id(context(), ne_binary()) -> context().
-spec set_device_id(context(), ne_binary()) -> context().
-spec set_auth_token(context(), ne_binary()) -> context().
-spec set_auth_doc(context(), kz_json:object()) -> context().
-spec set_auth_account_id(context(), ne_binary()) -> context().
-spec set_req_verb(context(), http_method()) -> context().
-spec set_req_data(context(), kz_json:object() | ne_binary()) -> context().
-spec set_req_files(context(), req_files()) -> context().
-spec set_req_nouns(context(), req_nouns()) -> context().
-spec set_req_headers(context(), cowboy:http_headers()) -> context().
-spec set_query_string(context(), kz_json:object()) -> context().
-spec set_req_id(context(), ne_binary()) -> context().
-spec set_doc(context(), api_object() | kz_json:objects()) -> context().
-spec set_load_merge_bypass(context(), api_binary()) -> context().
-spec set_start(context(), kz_now()) -> context().
-spec set_resp_data(context(), resp_data()) -> context().
-spec set_resp_status(context(), crossbar_status()) -> context().
-spec set_resp_expires(context(), kz_datetime()) -> context().
-spec set_api_version(context(), ne_binary()) -> context().
-spec set_resp_etag(context(), api_binary()) -> context().
-spec set_resp_envelope(context(), kz_json:object()) -> context().
-spec set_resp_headers(context(), kz_proplist()) -> context().
-spec add_resp_headers(context(), kz_proplist()) -> context().
-spec set_resp_header(context(), ne_binary(), ne_binary()) -> context().
-spec add_resp_header(context(), ne_binary(), ne_binary()) -> context().
-spec set_allow_methods(context(), http_methods()) -> context().
-spec set_allowed_methods(context(), http_methods()) -> context().
-spec set_method(context(), http_method()) -> context().
-spec set_req_json(context(), kz_json:object()) -> context().
-spec set_content_types_accepted(context(), crossbar_content_handlers()) -> context().
-spec set_content_types_provided(context(), crossbar_content_handlers()) -> context().
-spec set_languages_provided(context(), ne_binaries()) -> context().
-spec set_encodings_provided(context(), ne_binaries()) -> context().
-spec set_resp_error_code(context(), integer()) -> context().
-spec set_resp_error_msg(context(), api_binary()) -> context().
-spec set_magic_pathed(context(), boolean()) -> context().
-spec set_should_paginate(context(), boolean()) -> context().
-spec set_validation_errors(context(), kz_json:object()) -> context().
-spec set_port(context(), integer()) -> context().
-spec set_raw_host(context(), binary()) -> context().
-spec set_raw_path(context(), binary()) -> context().
-spec set_raw_qs(context(), binary()) -> context().
-spec set_client_ip(context(), ne_binary()) -> context().
-spec set_profile_id(context(), ne_binary()) -> context().

set_account_id(#cb_context{}=Context, AcctId) ->
    Context#cb_context{account_id=AcctId}.
set_account_name(#cb_context{}=Context, Name) ->
    Context#cb_context{account_name=Name}.
set_user_id(#cb_context{}=Context, UserId) ->
    Context#cb_context{user_id=UserId}.
set_device_id(#cb_context{}=Context, DeviceId) ->
    Context#cb_context{device_id=DeviceId}.
set_reseller_id(#cb_context{}=Context, AcctId) ->
    Context#cb_context{reseller_id=AcctId}.
set_account_db(#cb_context{}=Context, AcctDb) ->
    Context#cb_context{db_name=AcctDb}.
set_account_modb(#cb_context{}=Context, Year, Month) ->
    Context#cb_context{db_name=kazoo_modb:get_modb(account_id(Context), Year, Month)}.
set_account_modb(#cb_context{}=Context, AcctId, Year, Month) ->
    Context#cb_context{db_name=kazoo_modb:get_modb(AcctId, Year, Month)}.
set_auth_token_type(#cb_context{}=Context, AuthTokenType) ->
    Context#cb_context{auth_token_type=AuthTokenType}.
set_auth_token(#cb_context{}=Context, AuthToken) ->
    Context#cb_context{auth_token=AuthToken}.
set_auth_doc(#cb_context{}=Context, AuthDoc) ->
    Context#cb_context{auth_doc=AuthDoc}.
set_auth_account_id(#cb_context{}=Context, AuthBy) ->
    Context#cb_context{auth_account_id=AuthBy}.
set_req_verb(#cb_context{}=Context, ReqVerb) ->
    Context#cb_context{req_verb=ReqVerb}.
set_req_data(#cb_context{}=Context, ReqData) ->
    Context#cb_context{req_data=ReqData}.
set_req_files(#cb_context{}=Context, ReqFiles) ->
    Context#cb_context{req_files=ReqFiles}.
set_req_nouns(#cb_context{}=Context, ReqNouns) ->
    Context#cb_context{req_nouns=ReqNouns}.
set_req_headers(#cb_context{}=Context, ReqHs) ->
    Context#cb_context{req_headers=ReqHs}.
set_query_string(#cb_context{}=Context, Q) ->
    Context#cb_context{query_json=Q}.
set_req_id(#cb_context{}=Context, ReqId) ->
    Context#cb_context{req_id=ReqId}.
set_doc(#cb_context{}=Context, Doc) ->
    Context#cb_context{doc=Doc}.
set_load_merge_bypass(#cb_context{}=Context, JObj) ->
    Context#cb_context{load_merge_bypass=JObj}.
set_start(#cb_context{}=Context, Start) ->
    Context#cb_context{start=Start}.
set_resp_data(#cb_context{}=Context, RespData) ->
    Context#cb_context{resp_data=RespData}.
set_resp_status(#cb_context{}=Context, RespStatus) ->
    Context#cb_context{resp_status=RespStatus}.
set_resp_expires(#cb_context{}=Context, RespExpires) ->
    Context#cb_context{resp_expires=RespExpires}.
set_api_version(#cb_context{}=Context, ApiVersion) ->
    Context#cb_context{api_version=ApiVersion}.
set_resp_etag(#cb_context{}=Context, ETag) ->
    Context#cb_context{resp_etag=ETag}.
set_resp_envelope(#cb_context{}=Context, E) ->
    Context#cb_context{resp_envelope=E}.

set_allow_methods(#cb_context{}=Context, AMs) ->
    Context#cb_context{allow_methods=AMs}.
set_allowed_methods(#cb_context{}=Context, AMs) ->
    Context#cb_context{allowed_methods=AMs}.
set_method(#cb_context{}=Context, M) ->
    Context#cb_context{method=M}.

set_req_json(#cb_context{}=Context, RJ) ->
    Context#cb_context{req_json=RJ}.
set_content_types_accepted(#cb_context{}=Context, CTAs) ->
    Context#cb_context{content_types_accepted=CTAs}.
set_content_types_provided(#cb_context{}=Context, CTPs) ->
    Context#cb_context{content_types_provided=CTPs}.
set_languages_provided(#cb_context{}=Context, LP) ->
    Context#cb_context{languages_provided=LP}.
set_encodings_provided(#cb_context{}=Context, EP) ->
    Context#cb_context{encodings_provided=EP}.
set_magic_pathed(#cb_context{}=Context, MP) ->
    Context#cb_context{magic_pathed=kz_util:is_true(MP)}.
set_should_paginate(#cb_context{}=Context, SP) ->
    Context#cb_context{should_paginate=kz_util:is_true(SP)}.

set_resp_error_code(#cb_context{}=Context, Code) ->
    Context#cb_context{resp_error_code=Code}.
set_resp_error_msg(#cb_context{}=Context, Msg) ->
    Context#cb_context{resp_error_msg=Msg}.

set_resp_headers(#cb_context{resp_headers=Hs}=Context, Headers) ->
    Context#cb_context{resp_headers=lists:foldl(fun set_resp_header_fold/2, Hs, Headers)}.
set_resp_header(#cb_context{resp_headers=RespHeaders}=Context, K, V) ->
    Context#cb_context{resp_headers=lists:keystore(K, 1, RespHeaders, {K, V})}.
set_resp_header_fold({K, V}, Hs) -> lists:keystore(K, 1, Hs, {K, V}).

add_resp_headers(#cb_context{resp_headers=RespHeaders}=Context, Headers) ->
    Context#cb_context{resp_headers=lists:foldl(fun add_resp_header_fold/2, RespHeaders, Headers)}.

add_resp_header(#cb_context{resp_headers=RespHeaders}=Context, K, V) ->
    Context#cb_context{resp_headers=add_resp_header_fold({K, V}, RespHeaders)}.

-spec add_resp_header_fold({ne_binary(), any()}, kz_proplist()) -> kz_proplist().
add_resp_header_fold({K, V}, Hs) ->
    props:set_value(kz_util:to_lower_binary(K), V, Hs).

set_validation_errors(#cb_context{}=Context, Errors) ->
    Context#cb_context{validation_errors=Errors}.

set_port(#cb_context{}=Context, Value) ->
    Context#cb_context{port = Value}.

set_raw_host(#cb_context{}=Context, Value) ->
    Context#cb_context{raw_host = Value}.

set_raw_path(#cb_context{}=Context, Value) ->
    Context#cb_context{raw_path = Value}.

set_raw_qs(#cb_context{}=Context, Value) ->
    Context#cb_context{raw_qs = Value}.

set_client_ip(#cb_context{}=Context, Value) ->
    Context#cb_context{client_ip = Value}.

set_profile_id(#cb_context{}=Context, Value) ->
    Context#cb_context{profile_id = Value}.


-spec update_doc(context(), setter_fun_1()) -> context().
update_doc(#cb_context{doc=Doc}=Context, Updater) ->
    Context#cb_context{doc=Updater(Doc)}.

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

-spec add_attachment_content_type(context(), ne_binary(), ne_binary()) -> context().
add_attachment_content_type(#cb_context{}=Context, DocId, AttachmentId) ->
    Context1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION_ANY),
    case resp_status(Context1) of
        'success' ->
            maybe_add_content_type_provided(Context1, AttachmentId);
        _Status -> Context1
    end.

-spec maybe_add_content_type_provided(context(), ne_binary()) -> context().
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
-spec fetch(context(), any(), any()) -> any().

fetch(#cb_context{}=Context, Key) ->
    fetch(Context, Key, 'undefined').

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
-spec put_reqid(context()) -> api_binary().
put_reqid(#cb_context{req_id=ReqId}) ->
    kz_util:put_callid(ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
has_errors(#cb_context{validation_errors=JObj
                       ,resp_status='success'
                      }) ->
    (not kz_util:is_empty(JObj));
has_errors(#cb_context{}) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

response(#cb_context{resp_status='success'
                     ,resp_data=JObj
                    }) ->
    {'ok', JObj};
response(#cb_context{resp_error_code=Code
                     ,resp_error_msg=Msg
                     ,resp_data=DataJObj
                     ,validation_errors=ValidationJObj
                    }) ->
    ErrorCode = try kz_util:to_integer(Code) of
                    C -> C
                catch
                    _:_ -> 500
                end,
    ErrorMsg = case kz_util:is_empty(Msg) of
                   'false' -> kz_util:to_binary(Msg);
                   'true' -> <<"generic_error">>
               end,
    ErrorData = case {kz_util:is_empty(ValidationJObj), kz_util:is_empty(DataJObj)} of
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

-spec validate_request_data(ne_binary() | api_object(), context()) ->
                                   context().
-spec validate_request_data(ne_binary() | api_object(), context(), after_fun()) ->
                                   context().
-spec validate_request_data(ne_binary() | api_object(), context(), after_fun(), after_fun()) ->
                                   context().
validate_request_data('undefined', Context) ->
    passed(Context);
validate_request_data(<<_/binary>> = Schema, Context) ->
    case find_schema(Schema) of
        'undefined' ->
            passed(set_doc(Context, req_data(Context)));
        SchemaJObj ->
            validate_request_data(SchemaJObj, Context)
    end;
validate_request_data(SchemaJObj, Context) ->
    Strict = kapps_config:get_is_true(?CONFIG_CAT, <<"schema_strict_validation">>, 'false'),
    try kz_json_schema:validate(SchemaJObj
                               ,kz_json:public_fields(req_data(Context))
                               )
    of
        {'ok', JObj} ->
            passed(
              set_doc(Context, kz_json_schema:add_defaults(JObj, SchemaJObj))
             );
        {'error', Errors} when Strict ->
            lager:debug("request data did not validate against ~s: ~p", [kz_doc:id(SchemaJObj)
                                                                         ,Errors
                                                                        ]),
            failed(Context, Errors);
        {'error', Errors} ->
            maybe_fix_js_types(Context, SchemaJObj, Errors)
    catch
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            lager:debug("function clause failure"),
            kz_util:log_stacktrace(ST),
            Context#cb_context{resp_status='fatal'
                              ,resp_error_code=500
                              ,resp_data=kz_json:new()
                              ,resp_error_msg= <<"validation failed to run on the server">>
                              }
    end.

validate_request_data(Schema, Context, OnSuccess) ->
    validate_request_data(Schema, Context, OnSuccess, 'undefined').

validate_request_data(Schema, Context, OnSuccess, OnFailure) ->
    case validate_request_data(Schema, Context) of
        #cb_context{resp_status='success'}=C1 when is_function(OnSuccess) ->
            OnSuccess(C1);
        #cb_context{}=C2 when is_function(OnFailure) ->
            OnFailure(C2);
        Else -> Else
    end.

-spec failed(context(), jesse_error:error_reasons()) -> context().
-spec failed_error(jesse_error:error_reason(), context()) -> context().
failed(Context, Errors) ->
    Context1 = setters(Context
                      ,[{fun set_resp_error_code/2, 400}
                       ,{fun set_resp_status/2, 'error'}
                       ]
                      ),
    lists:foldl(fun failed_error/2
                ,Context1
                ,Errors
               ).

failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_length'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Minimum = kz_json:get_value(<<"minLength">>, FailedSchemaJObj),
    MinLen = kz_util:to_binary(Minimum),

    add_validation_error(
      FailedKeyPath
      ,<<"minLength">>
      ,kz_json:from_list(
         [{<<"message">>, <<"String must be at least ", MinLen/binary, " characters">>}
          ,{<<"target">>, Minimum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_max_length'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Maximum = kz_json:get_value(<<"maxLength">>, FailedSchemaJObj),
    MaxLen = kz_util:to_binary(Maximum),

    add_validation_error(
      FailedKeyPath
      ,<<"maxLength">>
      ,kz_json:from_list(
         [{<<"message">>, <<"String must not be more than ", MaxLen/binary, " characters">>}
          ,{<<"target">>, Maximum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'not_in_enum'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      FailedKeyPath
      ,<<"enum">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value not found in enumerated list of values">>}
          ,{<<"target">>, kz_json:get_value(<<"enum">>, FailedSchemaJObj, [])}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'not_minimum'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Minimum = kz_json:get_first_defined([<<"minimum">>, <<"exclusiveMinimum">>], FailedSchemaJObj),
    Min = kz_util:to_binary(Minimum),

    add_validation_error(
      FailedKeyPath
      ,<<"minimum">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value must be at least ", Min/binary>>}
          ,{<<"target">>, Minimum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'not_maximum'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Maximum = kz_json:get_first_defined([<<"maximum">>, <<"exclusiveMaximum">>], FailedSchemaJObj),
    Max = kz_util:to_binary(Maximum),

    add_validation_error(
      FailedKeyPath
      ,<<"maximum">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value must be at most ", Max/binary>>}
          ,{<<"target">>, Maximum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_size'
              ,FailedValue
              ,FailedKeyPath
             }, Context) ->
    Minimum = kz_json:get_value(<<"minItems">>, FailedSchemaJObj),
    Maximum = kz_json:get_value(<<"maxItems">>, FailedSchemaJObj),

    case length(FailedValue) of
        N when N < Minimum ->
            failed_error({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_min_items'
                          ,FailedValue
                          ,FailedKeyPath
                         }
                        ,Context
                        );
        N when N > Maximum ->
            failed_error({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_max_items'
                          ,FailedValue
                          ,FailedKeyPath
                         }
                        ,Context
                        )
    end;
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_items'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Minimum = kz_json:get_value(<<"minItems">>, FailedSchemaJObj),
    Min = kz_util:to_binary(Minimum),

    add_validation_error(
      FailedKeyPath
      ,<<"minItems">>
      ,kz_json:from_list(
         [{<<"message">>, <<"The list must have at least ", Min/binary, " items">>}
          ,{<<"target">>, Minimum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_max_items'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Maximum = kz_json:get_value(<<"maxItems">>, FailedSchemaJObj),
    Max = kz_util:to_binary(Maximum),

    add_validation_error(
      FailedKeyPath
      ,<<"maxItems">>
      ,kz_json:from_list(
         [{<<"message">>, <<"The list is more than ", Max/binary, " items">>}
          ,{<<"target">>, Maximum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_properties'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Minimum = kz_json:get_value(<<"minProperties">>, FailedSchemaJObj),
    Min = kz_util:to_binary(Minimum),

    add_validation_error(
      FailedKeyPath
      ,<<"minProperties">>
      ,kz_json:from_list(
         [{<<"message">>, <<"The object must have at least ", Min/binary, " keys">>}
          ,{<<"target">>, Minimum}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,{'not_unique', _Item}
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    lager:debug("item ~p is not unique", [_Item]),
    lager:debug("failed schema: ~p", [FailedSchemaJObj]),
    add_validation_error(
      FailedKeyPath
      ,<<"uniqueItems">>
      ,kz_json:from_list([{<<"message">>, <<"List of items is not unique">>}])
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_properties_allowed'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      FailedKeyPath
      ,<<"additionalProperties">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined properties">>}]
        )
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_items_allowed'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      FailedKeyPath
      ,<<"additionalItems">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined items">>}]
        )
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'no_match'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Pattern = kz_json:get_value(<<"pattern">>, FailedSchemaJObj),
    add_validation_error(
      FailedKeyPath
      ,<<"pattern">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Failed to match pattern '", Pattern/binary, "'">>}
          ,{<<"target">>, Pattern}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,'missing_required_property'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      FailedKeyPath
      ,<<"required">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Field is required but missing">>}]
        )
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,'missing_dependency'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      FailedKeyPath
      ,<<"dependencies">>
      ,kz_json:from_list([{<<"message">>, <<"Dependencies were not validated">>}])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'not_divisible'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    DivBy = kz_json:get_binary_value(<<"divisibleBy">>, FailedSchemaJObj),
    add_validation_error(
      FailedKeyPath
      ,<<"divisibleBy">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value not divisible by ", DivBy/binary>>}
          ,{<<"target">>, DivBy}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'not_allowed'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Disallow = get_disallow(FailedSchemaJObj),
    add_validation_error(
      FailedKeyPath
      ,<<"disallow">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value is disallowed by ", Disallow/binary>>}
          ,{<<"target">>, Disallow}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_type'
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    Types = get_types(FailedSchemaJObj),
    add_validation_error(
      FailedKeyPath
      ,<<"type">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Value did not match type(s): ", Types/binary>>}
          ,{<<"target">>, Types}
         ])
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,{'missing_required_property', FailKey}
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    add_validation_error(
      [FailedKeyPath, FailKey]
      ,<<"required">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Field is required but missing">>}]
        )
      ,Context
     );
failed_error({'data_invalid'
              ,_FailedSchemaJObj
              ,FailMsg
              ,_FailedValue
              ,FailedKeyPath
             }, Context) ->
    lager:debug("failed message: ~p", [FailMsg]),
    lager:debug("failed schema: ~p", [_FailedSchemaJObj]),
    lager:debug("failed value: ~p", [_FailedValue]),
    lager:debug("failed keypath: ~p", [FailedKeyPath]),
    add_validation_error(
      FailedKeyPath
      ,kz_util:to_binary(FailMsg)
      ,kz_json:from_list([{<<"message">>, <<"failed to validate">>}])
      ,Context
     ).

-spec get_disallow(kz_json:object()) -> ne_binary().
get_disallow(JObj) ->
    case kz_json:get_value(<<"disallow">>, JObj) of
        <<_/binary>> = Disallow -> Disallow;
        Disallows when is_list(Disallows) -> kz_util:join_binary(Disallows)
    end.

-spec get_types(kz_json:object()) -> ne_binary().
get_types(JObj) ->
    case kz_json:get_first_defined([<<"type">>, <<"types">>], JObj) of
        <<_/binary>> = Type -> Type;
        Types when is_list(Types) -> kz_util:join_binary(Types);
        _TypeSchema -> <<"type schema">>
    end.

-spec passed(context()) -> context().
-spec passed(context(), crossbar_status()) -> context().
passed(#cb_context{resp_status='error'}=Context) ->
    passed(Context, 'error');
passed(Context) ->
    passed(Context, 'success').

passed(#cb_context{req_data=Data}=Context, Status) ->
    case kz_doc:id(Data) of
        'undefined' ->
            Context#cb_context{resp_status = Status};
        Id ->
            Context#cb_context{resp_status = Status
                               ,doc = kz_doc:set_id(doc(Context), Id)
                              }
    end.

-spec find_schema(ne_binary()) -> api_object().
find_schema(<<_/binary>> = Schema) ->
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
-spec add_system_error(atom() | binary(), ne_binary() | kz_json:object(), context()) -> context().
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
    build_system_error(500, Error, kz_util:to_binary(Error), Context).

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
    Message = <<"bulk operations do not support documents of type ", (kz_util:to_binary(Type))/binary>>,
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

add_system_error(Code, Error, JObj, Context) ->
    build_system_error(Code, Error, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_system_error(integer(), atom() | ne_binary(), ne_binary() | kz_json:object(), cb_context:context()) ->
                                cb_context:context().
build_system_error(Code, Error, JObj, Context) ->
    ApiVersion = ?MODULE:api_version(Context),
    Message = build_error_message(ApiVersion, JObj),
    Context#cb_context{resp_status='error'
                       ,resp_error_code=Code
                       ,resp_data=Message
                       ,resp_error_msg=kz_util:to_binary(Error)
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
add_validation_error(Property, <<"type">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"items">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"required">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minimum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maximum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maxItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"uniqueItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"pattern">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minLength">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maxLength">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maxSize">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"enum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"format">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"divisibleBy">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);

%% Not unique within the datastore
add_validation_error(Property, <<"unique">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% User is not authorized to update the property
add_validation_error(Property, <<"forbidden">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Date range is invalid, too small, or too large
add_validation_error(Property, <<"date_range">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Value was required to locate a resource, but failed (like account_name)
add_validation_error(Property, <<"not_found">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Value's keys didn't match property
add_validation_error(Property, <<"patternProperties">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"disabled">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"expired">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
% Generic
add_validation_error(Property, <<"invalid">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);

add_validation_error(Property, Code, Message, Context) ->
    lager:warning("UNKNOWN ERROR CODE: ~p", [Code]),
    add_depreciated_validation_error(Property, Code, Message, Context).

add_depreciated_validation_error(<<"account">>, <<"expired">>, Message, Context) ->
    add_depreciated_validation_error(<<"account">>, <<"expired">>, Message, Context, 423, <<"locked">>);
add_depreciated_validation_error(<<"account">>, <<"disabled">>, Message, Context) ->
    add_depreciated_validation_error(<<"account">>, <<"disabled">>, Message, Context, 423, <<"locked">>);
add_depreciated_validation_error(Property, Code, Message, Context) ->
    add_depreciated_validation_error(Property, Code, Message, Context, resp_error_code(Context), resp_error_msg(Context)).

add_depreciated_validation_error(Property, Code, Message, Context, 'undefined', ErrMsg) ->
    add_depreciated_validation_error(Property, Code, Message, Context, 400, ErrMsg);
add_depreciated_validation_error(Property, Code, Message, Context, ErrorCode, 'undefined') ->
    add_depreciated_validation_error(Property, Code, Message, Context, ErrorCode, <<"invalid request">>);
add_depreciated_validation_error(Property, Code, Message, Context, ErrorCode, <<"init failed">>) ->
    add_depreciated_validation_error(Property, Code, Message, Context, ErrorCode, <<"invalid request">>);
add_depreciated_validation_error(<<_/binary>> = Property, Code, Message, Context, ErrCode, ErrMsg) ->
    add_depreciated_validation_error([Property], Code, Message, Context, ErrCode, ErrMsg);
add_depreciated_validation_error(Property, Code, Message, Context, ErrCode, ErrMsg) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    ApiVersion = ?MODULE:api_version(Context),

    JObj = cb_context:validation_errors(Context),
    Error = build_error_message(ApiVersion, Message),

    Key = kz_util:join_binary(Property, <<".">>),
    Context#cb_context{validation_errors=kz_json:set_value([Key, Code], Error, JObj)
                       ,resp_status='error'
                       ,resp_error_code=ErrCode
                       ,resp_data=kz_json:new()
                       ,resp_error_msg=ErrMsg
                      }.

-spec build_error_message('v1', ne_binary() | kz_json:object()) ->
                                 ne_binary();
                         (ne_binary(), ne_binary() | kz_json:object()) ->
                                 kz_json:object().
build_error_message(?VERSION_1, Message) when is_binary(Message) ->
    Message;
build_error_message(?VERSION_1, JObj) ->
    kz_json:get_value(<<"message">>, JObj);
build_error_message(_Version, Message) when is_binary(Message) ->
    kz_json:from_list([{<<"message">>, Message}]);
build_error_message(_Version, JObj) ->
    JObj.

-spec maybe_fix_js_types(cb_context:context(), kz_json:object(), jesse_error:error_reasons()) ->
                                cb_context:context().
maybe_fix_js_types(Context, SchemaJObj, Errors) ->
    JObj = req_data(Context),
    case lists:foldl(fun maybe_fix_js_type/2, JObj, Errors) of
        JObj ->
            lager:debug("request data did not validate against ~s: ~p", [kz_doc:id(SchemaJObj)
                                                                         ,Errors
                                                                        ]),
            failed(Context, Errors);
        NewJObj ->
            validate_request_data(SchemaJObj, set_req_data(Context, NewJObj))
    end.

-spec maybe_fix_js_type(jesse_error:error_reason(), kz_json:object()) ->
                               kz_json:object().
maybe_fix_js_type({'data_invalid', SchemaJObj, 'wrong_type', Value, Key}, JObj) ->
    case kz_json:get_value(<<"type">>, SchemaJObj) of
        <<"integer">> -> maybe_fix_js_integer(Key, Value, JObj);
        _Type -> JObj
    end;
maybe_fix_js_type(_, JObj) -> JObj.

-spec maybe_fix_js_integer(kz_json:key(), kz_json:json_term(), kz_json:object()) ->
                                  kz_json:object().
maybe_fix_js_integer(Key, Value, JObj) ->
    try kz_util:to_integer(Value) of
        V -> kz_json:set_value(Key, V, JObj)
    catch
        _E:_R ->
            lager:debug("error converting value to integer ~p : ~p : ~p"
                       ,[Value, _E, _R]
                       ),
            JObj
    end.
