%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Provides access to stored call recordings.
%%%
%%% @author OnNet (Kirill Sysoev [github.com/onnet])
%%% @author Dinkor (Andrew Korniliv [github.com/dinkor])
%%% @author Lazedo (Luis Azedo [github.com/2600hz])
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_recordings).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,validate/1, validate/2
        ,delete/2, patch/2
        ,to_json/1
        ,to_csv/1
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"recordings/crossbar_listing">>).
-define(CB_LIST_BY_OWNERID, <<"recordings/listing_by_user">>).

-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"mpeg">>}
                          ,{<<"audio">>, <<"mp3">>}
                          ]).

-define(PVT_TYPE, <<"call_recording">>).

-define(COLLECTION, <<"collection">>).
-define(COLLECTION_RECORDINGS, <<"recordings">>).

-define(COLUMNS
       ,[{<<"id">>, fun col_id/2}
        ,{<<"call_id">>, fun col_call_id/2}
        ,{<<"caller_id_number">>, fun col_caller_id_number/2}
        ,{<<"caller_id_name">>, fun col_caller_id_name/2}
        ,{<<"callee_id_number">>, fun col_callee_id_number/2}
        ,{<<"callee_id_name">>, fun col_callee_id_name/2}
        ,{<<"cdr_id">>, fun col_cdr_id/2}
        ,{<<"content_type">>, fun col_content_type/2}
        ,{<<"custom_channel_vars">>, fun col_custom_channel_vars/2}
        ,{<<"description">>, fun col_description/2}
        ,{<<"direction">>, fun col_direction/2}
        ,{<<"duration">>, fun col_duration/2}
        ,{<<"duration_ms">>, fun col_duration_ms/2}
        ,{<<"from">>, fun col_from/2}
        ,{<<"interaction_id">>, fun col_interaction_id/2}
        ,{<<"media_source">>, fun col_media_source/2}
        ,{<<"media_type">>, fun col_media_type/2}
        ,{<<"name">>, fun col_name/2}
        ,{<<"origin">>, fun col_origin/2}
        ,{<<"owner_id">>, fun col_owner_id/2}
        ,{<<"request">>, fun col_request/2}
        ,{<<"soft_delete">>, fun col_soft_delete/2}
        ,{<<"soft_restore">>, fun col_soft_restore/2}
        ,{<<"source_type">>, fun col_source_type/2}
        ,{<<"start">>, fun col_start/2}
        ,{<<"to">>, fun col_to/2}
        ,{<<"url">>, fun col_url/2}
        ]).

-define(COLUMNS_RESELLER
       ,[
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.recordings">>, 'allowed_methods'}
               ,{<<"*.resource_exists.recordings">>, 'resource_exists'}
               ,{<<"*.content_types_provided.recordings">>, 'content_types_provided'}
               ,{<<"*.validate.recordings">>, 'validate'}
               ,{<<"*.execute.patch.recordings">>, 'patch'}
               ,{<<"*.execute.delete.recordings">>, 'delete'}
               ],
    _ = crossbar_bindings:bind(<<"*.to_json.get.recordings">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.recordings">>, ?MODULE, 'to_json'),
    cb_modules_util:bind(?MODULE, Bindings).

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?COLLECTION) ->
    [?HTTP_PATCH];
allowed_methods(_RecordingId) ->
    [?HTTP_GET, ?HTTP_DELETE, ?HTTP_PATCH].

-spec to_json(cb_cowboy_payload()) -> cb_cowboy_payload().
to_json({Req, Context}) ->
    {Req, to_response(Context, <<"json">>, cb_context:req_nouns(Context))}.

-spec to_csv(cb_cowboy_payload()) -> cb_cowboy_payload().
to_csv({Req, Context}) ->
    {Req, to_response(Context, <<"csv">>, cb_context:req_nouns(Context))}.

-spec to_response(cb_context:context(), kz_term:ne_binary(), req_nouns()) ->
          cb_context:context().
to_response(Context, RespType, [{<<"recordings">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    load_chunked_recordings(Context, RespType);
to_response(Context, _, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Loads Call Recordings docs from database and normalized them.
%% @end
%%------------------------------------------------------------------------------
-spec load_chunked_recordings(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_chunked_recordings(Context, RespType) ->
    load_chunked_recordings(Context, RespType, cb_context:resp_data(Context)).

-spec load_chunked_recordings(cb_context:context(), kz_term:ne_binary(), kz_json:objects()) -> cb_context:context().
load_chunked_recordings(Context, RespType, RespData) ->
    AccountId = cb_context:account_id(Context),
    Fun = fun(JObj, Acc) -> split_to_modbs(AccountId, kz_doc:id(JObj), Acc) end,
    MapIds = lists:foldl(Fun, #{}, RespData),

    C1 = cb_context:set_resp_data(Context, []),
    try maps:fold(fun(Db, Ids, C) -> load_chunked_recording_ids(C, RespType, Db, Ids) end, C1, MapIds)
    catch
        _T:_E ->
            cb_context:add_system_error('datastore_fault', Context)
    end.

%% if request is not chunked, map Ids to MODBs
-spec split_to_modbs(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> map().
split_to_modbs(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)=Id, Map) ->
    Db = kazoo_modb:get_modb(AccountId, Year, Month),
    maps:update_with(Db, fun(List) -> List ++ [Id] end, [Id], Map).

-spec load_chunked_recording_ids(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> cb_context:context().
load_chunked_recording_ids(Context, RespType, Db, Ids) ->
    load_chunked_recording_ids(Context, RespType, Db, Ids, cb_context:resp_status(Context)).

load_chunked_recording_ids(Context, RespType, Db, Ids, 'success') ->
    case kz_datamgr:open_docs(Db, Ids, [{'doc_type', kzd_call_recordings:type()}]) of
        {'ok', Results} ->
            Resp0 = [normalize_recording(Context, RespType, Result)
                     || Result <- Results,
                        %% if there are no filters, include doc
                        %% otherwise run filters against doc for inclusion
                        crossbar_filter:by_doc(kz_json:get_json_value(<<"doc">>, Result), Context)
                    ],
            RespAcc = cb_context:resp_data(Context),
            maybe_add_csv_header(Context, RespType, RespAcc ++ Resp0);
        {'error', Reason} ->
            lager:debug("failed to load cdrs doc from ~s: ~p", [Db, Reason]),
            crossbar_doc:handle_datamgr_errors(Reason, <<"load_cdrs">>, Context)
    end;
load_chunked_recording_ids(Context, _RespType, _Db, _Ids, _Status) ->
    Context.

-spec normalize_recording(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object() | kz_term:binary().
normalize_recording(Context, <<"json">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    kz_json:from_list(
      props:filter_empty(
        [{K, F(JObj, Context)} || {K, F} <- csv_rows(Context)]));
normalize_recording(Context, <<"csv">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    <<(kz_binary:join([F(JObj, Context) || {_, F} <- csv_rows(Context)], <<",">>))/binary, "\r\n">>.

-spec maybe_add_csv_header(cb_context:context(), kz_term:ne_binary(), kz_json:objects() | kz_term:binaries()) -> cb_context:context().
maybe_add_csv_header(Context, _, []) ->
    Context;
maybe_add_csv_header(Context, <<"json">>, Data) ->
    cb_context:set_resp_data(Context, Data);
maybe_add_csv_header(Context, <<"csv">>, [Head | Tail]=Data) ->
    case cb_context:fetch(Context, 'chunking_started') of
        'true' ->
            cb_context:set_resp_data(Context, Data);
        'false' ->
            CSVHeader = kz_binary:join([K || {K, _Fun} <- csv_rows(Context)], <<",">>),
            cb_context:set_resp_data(Context, [<<CSVHeader/binary, "\r\n", Head/binary>> | Tail])
    end.

%% -spec csv_rows(cb_context:context()) -> [{kz_term:ne_binary(), csv_column_fun()}].
csv_rows(Context) ->
    case cb_context:fetch(Context, 'is_reseller', 'false') of
        'false' -> ?COLUMNS;
        'true' -> ?COLUMNS ++ ?COLUMNS_RESELLER
    end.

%% see csv_column_fun() for specs for each function here
col_id(JObj, _Context) -> kz_doc:id(JObj, <<>>).
col_call_id(JObj, _Context) -> kzd_cdrs:call_id(JObj, <<>>).
col_caller_id_number(JObj, _Context) -> kzd_cdrs:caller_id_number(JObj, <<>>).
col_caller_id_name(JObj, _Context) -> kzd_cdrs:caller_id_name(JObj, <<>>).
col_callee_id_number(JObj, _Context) -> kzd_cdrs:callee_id_number(JObj, <<>>).
col_callee_id_name(JObj, _Context) -> kzd_cdrs:callee_id_name(JObj, <<>>).
col_cdr_id(JObj, _Context) -> kz_json:get_value(<<"cdr_id">>, JObj, <<>>).
col_content_type(JObj, _Context) -> kz_json:get_value(<<"content_type">>, JObj, <<>>).
col_custom_channel_vars(JObj, _Context) -> kz_json:get_value(<<"custom_channel_vars">>, JObj, <<>>).
col_description(JObj, _Context) -> kz_json:get_value(<<"description">>, JObj, <<>>).
col_direction(JObj, _Context) -> kz_json:get_value(<<"direction">>, JObj, <<>>).
col_duration(JObj, _Context) -> kz_json:get_value(<<"duration">>, JObj, <<>>).
col_duration_ms(JObj, _Context) -> kz_json:get_value(<<"duration_ms">>, JObj, <<>>).
col_from(JObj, _Context) -> kz_json:get_value(<<"from">>, JObj, <<>>).
col_interaction_id(JObj, _Context) -> kz_json:get_value(<<"interaction_id">>, JObj, <<>>).
col_media_source(JObj, _Context) -> kz_json:get_value(<<"media_source">>, JObj, <<>>).
col_media_type(JObj, _Context) -> kz_json:get_value(<<"media_type">>, JObj, <<>>).
col_name(JObj, _Context) -> kz_json:get_value(<<"name">>, JObj, <<>>).
col_origin(JObj, _Context) -> kz_json:get_value(<<"origin">>, JObj, <<>>).
col_owner_id(JObj, _Context) -> kz_json:get_value(<<"owner_id">>, JObj, <<>>).
col_request(JObj, _Context) -> kz_json:get_value(<<"request">>, JObj, <<>>).
col_soft_delete(JObj, _Context) -> kz_json:get_value(<<"soft_delete">>, JObj, <<>>).
col_soft_restore(JObj, _Context) -> kz_json:get_value(<<"soft_restore">>, JObj, <<>>).
col_source_type(JObj, _Context) -> kz_json:get_value(<<"source_type">>, JObj, <<>>).
col_start(JObj, _Context) -> kz_json:get_value(<<"start">>, JObj, <<>>).
col_to(JObj, _Context) -> kz_json:get_value(<<"to">>, JObj, <<>>).
col_url(JObj, _Context) -> kz_json:get_value(<<"url">>, JObj, <<>>).

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_RecordingId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's Accept header).
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, _RecordingId) ->
    content_types_provided_for_download(Context, cb_context:req_verb(Context)).

-spec content_types_provided_for_download(cb_context:context(), http_method()) -> cb_context:context().
content_types_provided_for_download(Context, ?HTTP_GET) ->
    CTP = [{'to_json', ?JSON_CONTENT_TYPES}
          ,{'to_binary', ?MEDIA_MIME_TYPES}
          ],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided_for_download(Context, _Verb) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    QS = cb_context:query_string(Context),
    case  kz_json:get_value(<<"include">>, QS) == <<"soft_delete">> of
        true ->
            recording_summary(Context);
        false ->
            Values = [{<<"key_missing">>, <<"soft_delete">>}],
            AdjustedQS = kz_json:set_values(Values, QS),
            AdjustedContext = cb_context:set_query_string(Context, AdjustedQS),
            recording_summary(AdjustedContext)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?COLLECTION) ->
    validate_collection_request(Context);
validate(Context, RecordingId) ->
    validate_recording(Context, RecordingId, cb_context:req_verb(Context)).

validate_recording(Context, RecordingId, ?HTTP_GET) ->
    case action_lookup(Context) of
        'read' ->
            load_recording_doc(Context, RecordingId);
        'download' ->
            load_recording_binary(Context, RecordingId)
    end;
validate_recording(Context, RecordingId, ?HTTP_PATCH) ->
    validate_patch(RecordingId, Context);
validate_recording(Context, RecordingId, ?HTTP_DELETE) ->
    load_recording_doc(Context, RecordingId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(RecordingId, Context) ->
    patch_and_validate(RecordingId, Context, fun validate_request/2).

-spec patch_and_validate(kz_term:ne_binary(), cb_context:context(), fun()) ->
          cb_context:context().
patch_and_validate(Id, Context, ValidateFun) ->
    Context1 = load_recording_doc(Context, Id),
    patch_and_validate_doc(Id, Context1, ValidateFun, cb_context:resp_status(Context1)).

-spec patch_and_validate_doc(kz_term:ne_binary(), cb_context:context(), fun(), crossbar_status()) ->
          cb_context:context().
patch_and_validate_doc(Id, Context, ValidateFun, 'success') ->
    PatchedJObj = patch_the_doc(cb_context:req_data(Context), cb_context:doc(Context)),
    Context1 = cb_context:set_req_data(Context, PatchedJObj),
    ValidateFun(Id, cb_context:set_doc(Context1, PatchedJObj));
patch_and_validate_doc(Id, Context, ValidateFun, _RespStatus) ->
    ValidateFun(Id, Context).

-spec patch_the_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
patch_the_doc(RequestData, ExistingDoc) ->
    PubJObj = kz_doc:public_fields(RequestData),
    kz_json:merge(fun kz_json:merge_left/2, PubJObj, ExistingDoc).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

%%-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
%%validate_request(_RecordingId, Context) ->
%%    Context.
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(RecordingId, Context) ->
    ValidateFuns = [
                    fun validate_schema/2
                   ],
    lists:foldl(fun(F, C) -> F(RecordingId, C) end
               ,Context
               ,ValidateFuns
               ).

-spec validate_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_schema(RecordingId, Context) ->
    lager:debug("validating call_recording payload"),
    OnSuccess = fun(C) ->
                        lager:debug("account payload is valid"),
                        on_successful_validation(RecordingId, C)
                end,
    cb_context:validate_request_data(<<"call_recordings">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation(_RecordingId, Context) ->
    Context.


%%------------------------------------------------------------------------------
%% @doc Validates a collection-type recordings field..
%% @end
%%------------------------------------------------------------------------------

-spec validate_collection_request(cb_context:context()) -> cb_context:context().
validate_collection_request(Context) ->
    Recordings = kz_json:get_value(?COLLECTION_RECORDINGS, cb_context:req_data(Context)),
    validate_collection_request(Context, Recordings).

-spec validate_collection_request(cb_context:context(), any()) -> cb_context:context().
validate_collection_request(Context, 'undefined') ->
    Msg = kz_json:from_list([{<<"message">>, <<"list of recordings missing">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_RECORDINGS, <<"required">>, Msg, Context);
validate_collection_request(Context, []) ->
    Msg = kz_json:from_list([{<<"message">>, <<"minimum 1 recording required">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_RECORDINGS, <<"minimum">>, Msg, Context);
validate_collection_request(Context, Recordings)
  when is_list(Recordings) ->
    UniqNomalised = lists:usort(Recordings),
    case length(Recordings) =:= length(UniqNomalised) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"some recordings appear twice">>}
                                    ]),
            cb_context:add_validation_error(?COLLECTION_RECORDINGS, <<"uniqueItems">>, Msg, Context)
    end;
validate_collection_request(Context, _E) ->
    Msg = kz_json:from_list([{<<"message">>, <<"recordings must be a list">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_RECORDINGS, <<"type">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, ?COLLECTION) ->
    Results = collection_process(Context, ?HTTP_PATCH),
    crossbar_util:response(Results, Context);
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _RecordingId) ->
    crossbar_doc:delete(Context).

-spec recording_summary(cb_context:context()) -> cb_context:context().
recording_summary(Context) ->
    UserId = cb_context:user_id(Context),
    ViewName = get_view_name(UserId),
    Options = [{'is_chunked', 'true'}
              ,{'mapper', fun summary_doc_fun/2}
              ,{'range_start_keymap', [UserId]}
              ,{'range_end_keymap', fun(Ts) -> build_end_key(Ts, UserId) end}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec summary_doc_fun(kz_json:object(), kz_json:objects()) -> kz_json:objects().
summary_doc_fun(View, Acc) ->
    Recording = kz_json:get_json_value(<<"doc">>, View),
    Attachments = kz_doc:attachments(Recording, kz_json:new()),
    ContentTypes = kz_json:foldl(fun attachment_content_type/3, [], Attachments),

    [kz_json:set_value([<<"_read_only">>, <<"content_types">>]
                      ,lists:usort(ContentTypes)
                      ,Recording
                      )
     | Acc
    ].

-spec attachment_content_type(kz_json:key(), kz_json:object(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
attachment_content_type(_Name, Meta, CTs) ->
    [kz_json:get_ne_binary_value(<<"content_type">>, Meta) | CTs].

-spec build_end_key(kz_time:gregorian_seconds(), kz_term:api_ne_binary()) -> kazoo_data:range_key().
build_end_key(Timestamp, 'undefined') -> [Timestamp, kz_json:new()];
build_end_key(Timestamp, UserId) -> [UserId, Timestamp, kz_json:new()].

-spec get_view_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_view_name('undefined') -> ?CB_LIST;
get_view_name(_) -> ?CB_LIST_BY_OWNERID.

-spec load_recording_doc(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_recording_doc(Context, ?MATCH_MODB_PREFIX(Year, Month, _) = RecordingId) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_db_name(Context, AccountDb),
    crossbar_doc:load({<<"call_recording">>, RecordingId}, Context1, ?TYPE_CHECK_OPTION(<<"call_recording">>));
load_recording_doc(Context, Id) ->
    crossbar_util:response_bad_identifier(Id, Context).

-spec load_recording_binary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_recording_binary(Context, ?MATCH_MODB_PREFIX(Year, Month, _) = DocId) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_db_name(Context, AccountDb),
    do_load_recording_binary(Context1, DocId).

-spec do_load_recording_binary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
do_load_recording_binary(Context, DocId) ->
    Context1 = crossbar_doc:load({<<"call_recording">>, DocId}, Context, ?TYPE_CHECK_OPTION(<<"call_recording">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            do_load_recording_binary_attachment(Context1, DocId);
        _Status -> Context1
    end.

-spec do_load_recording_binary_attachment(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
do_load_recording_binary_attachment(Context, DocId) ->
    case kz_doc:attachment_names(cb_context:doc(Context)) of
        [] ->
            cb_context:add_system_error('bad_identifier'
                                       ,kz_json:from_list([{<<"details">>, DocId}])
                                       ,Context
                                       );
        [AName | _] ->
            LoadedContext = crossbar_doc:load_attachment({<<"call_recording">>, DocId}
                                                        ,AName
                                                        ,?TYPE_CHECK_OPTION(<<"call_recording">>)
                                                        ,Context
                                                        ),

            set_resp_headers(LoadedContext
                            ,AName
                            ,kz_doc:attachment(cb_context:doc(Context), AName)
                            )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_resp_headers(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
set_resp_headers(Context, AName, Attachment) ->
    Headers = #{<<"content-disposition">> => get_disposition(AName, Context)
               ,<<"content-type">> => kz_json:get_ne_binary_value(<<"content_type">>, Attachment)
               },
    cb_context:add_resp_headers(Context, Headers).

-spec get_disposition(kz_term:ne_binary(), cb_context:context()) -> kz_term:ne_binary().
get_disposition(MediaName, Context) ->
    case kz_json:is_true(<<"inline">>, cb_context:query_string(Context), 'false') of
        'false' -> <<"attachment; filename=", MediaName/binary>>;
        'true' -> <<"inline; filename=", MediaName/binary>>
    end.

-spec action_lookup(cb_context:context()) -> atom().
action_lookup(Context) ->
    Acceptable = acceptable_content_types(Context),
    action_lookup(Acceptable, accept_values(Context)).

-spec action_lookup(kz_term:proplist(), media_values()) -> atom().
action_lookup(_, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)|_]) ->
    'read';
action_lookup(_, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)|_]) ->
    'read';
action_lookup(_, [?MEDIA_VALUE(<<"*">>, <<"*">>, _, _, _)|_]) ->
    lager:debug("catch-all accept header, using json"),
    'read';
action_lookup(Acceptable, [?MEDIA_VALUE(Type, SubType, _, _, _)|Accepts]) ->
    case is_acceptable_accept(Acceptable, Type, SubType) of
        'false' ->
            lager:debug("unknown accept header: ~s/~s", [Type, SubType]),
            action_lookup(Acceptable, Accepts);
        'true' ->
            lager:debug("accept header: ~s/~s", [Type, SubType]),
            'download'
    end;
action_lookup(_, []) ->
    lager:debug("no accept headers, using json"),
    'read'.

-spec accept_values(cb_context:context()) -> media_values().
accept_values(Context) ->
    AcceptValue = cb_context:req_header(Context, <<"accept">>),
    Tunneled = cb_context:req_value(Context, <<"accept">>),
    media_values(AcceptValue, Tunneled).

-spec media_values(kz_term:api_binary(), kz_term:api_binary()) -> media_values().
media_values('undefined', 'undefined') ->
    lager:debug("no accept headers, assuming JSON"),
    [?MEDIA_VALUE(<<"application">>, <<"json">>)];
media_values(AcceptValue, 'undefined') ->
    case cb_modules_util:parse_media_type(AcceptValue) of
        {'error', 'badarg'} -> media_values('undefined', 'undefined');
        AcceptValues -> lists:reverse(lists:keysort(2, AcceptValues))
    end;
media_values(AcceptValue, Tunneled) ->
    case cb_modules_util:parse_media_type(Tunneled) of
        {'error', 'badarg'} -> media_values(AcceptValue, 'undefined');
        TunneledValues ->
            lager:debug("using tunneled accept value ~s", [Tunneled]),
            lists:reverse(lists:keysort(2, TunneledValues))
    end.

-spec acceptable_content_types(cb_context:context()) -> kz_term:proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec is_acceptable_accept(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:member({Type,SubType}, Acceptable).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collection_process(cb_context:context(), kz_term:ne_binary() | http_method()) -> recordings:ret().
collection_process(Context, Action) ->
    ReqData = cb_context:req_data(Context),
    Recordings = kz_json:get_list_value(<<"recordings">>, ReqData),
    Context1 = cb_context:set_req_data(Context, kz_json:delete_key(<<"recordings">>, ReqData)),
    recordings_action(Context1, Action, Recordings).

recordings_action(Context, ?HTTP_PATCH, Recordings) ->
    Fun = fun(RecordingId, {Success, Failure}) ->
                  Ctx1 = validate_patch(RecordingId, Context),
                  case cb_context:has_errors(Ctx1) of
                      'true' ->
                          {'error', {ErrorCode, ErrorMsg, _ErrorData}} = cb_context:response(Ctx1),
                          Resp = kz_json:from_list([ {<<"cause">>, RecordingId}
                                                   ,{<<"code">>, ErrorCode}
                                                   ,{<<"message">>, ErrorMsg}]),
                          {Success, kz_json:set_value(RecordingId, Resp, Failure)};
                      'false' ->
                          Ctx2 = crossbar_doc:save(Ctx1),
                          Doc = cb_context:doc(Ctx2),
                          {kz_json:set_value(RecordingId, Doc, Success), Failure}
                  end
          end,
    {Success, Failure} = lists:foldl(Fun,
                                     {kz_json:new(), kz_json:new()},
                                     Recordings),
    kz_json:merge(
      case kz_json:is_empty(Success) of
          true -> kz_json:new();
          false -> kz_json:set_value(<<"success">>, Success, kz_json:new())
      end,
      case kz_json:is_empty(Failure) of
          true -> kz_json:new();
          false -> kz_json:set_value(<<"failure">>, Failure, kz_json:new())
      end
     ).

