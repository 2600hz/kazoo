%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account module
%%%
%%% Store/retrieve media files
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_media).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,content_types_provided/3
         ,content_types_accepted/3
         ,get/3
         ,put/1
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).

-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"x-wav">>}
                           ,{<<"audio">>, <<"mpeg">>}
                           ,{<<"audio">>, <<"mp3">>}
                           ,{<<"audio">>, <<"ogg">>}
                           ,{<<"application">>, <<"octet-stream">>}
                           ,{<<"application">>, <<"base64">>}
                           ,{<<"application">>, <<"x-base64">>}
                          ]).

-define(PVT_TYPE, <<"media">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.media">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.media">>, ?MODULE, content_types_accepted),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.media">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.media">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.media">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.media">>, ?MODULE, get),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.media">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.media">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.media">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_MediaID) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_MediaID, ?BIN_DATA) ->
    ['GET', 'POST'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, ?BIN_DATA) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
content_types_provided(#cb_context{db_name=Db, req_verb = <<"get">>}=Context, MediaID, ?BIN_DATA) when is_binary(Db) ->
    lager:debug("open media doc ~s / ~s", [Db, MediaID]),
    case couch_mgr:open_doc(Db, MediaID) of
        {error, _} -> Context;
        {ok, JObj} ->
            lager:debug("media: ~p", [JObj]),
            [Type, SubType] = binary:split(content_type_of(JObj), <<"/">>),
            lager:debug("getting media of type ~s/~s", [Type, SubType]),
            Context#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
    end.

-spec content_types_accepted/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = <<"post">>}=Context, _MediaID, ?BIN_DATA) ->
    CTA = [{from_binary, ?MEDIA_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    lookup_media(Context);
validate(#cb_context{req_verb = <<"put">>, req_data=Data}=Context) ->
    Name = wh_json:get_value(<<"name">>, Data),

    case Name =/= undefined andalso lookup_media_by_name(Name, Context) of
        false ->
            crossbar_util:response_invalid_data([<<"name">>], Context);
        #cb_context{resp_status=success, doc=[Doc|_], resp_headers=RHs}=Context1 ->
            DocID = wh_json:get_value(<<"id">>, Doc),
            lager:debug("media with name ~s found in doc ~s", [Name, Doc]),
            Context1#cb_context{resp_headers=[{"Location", DocID} | RHs]};
        _ ->
            lager:debug("media name ~s unknown, create meta", [Name]),
            create_media_meta(Data, Context)
    end.

validate(#cb_context{req_verb = <<"get">>}=Context, MediaID) ->
    get_media_doc(MediaID, Context);
validate(#cb_context{req_verb = <<"post">>, req_data=Data}=Context, MediaID) ->
    case wh_json:get_value(<<"name">>, Data) =/= undefined of
        true ->
            crossbar_doc:load_merge(MediaID, Data, Context);
        false ->
            crossbar_util:response_invalid_data([<<"name">>], Context)
    end;
validate(#cb_context{req_verb = <<"delete">>, req_data=_Data}=Context, MediaID) ->
    get_media_doc(MediaID, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, MediaID, ?BIN_DATA) ->
    lager:debug("fetch media contents"),
    get_media_binary(MediaID, Context);
validate(#cb_context{req_verb = <<"post">>, req_files=[]}=Context, _MediaID, ?BIN_DATA) ->
    lager:debug("No files in request to save attachment"),
    crossbar_util:response_invalid_data([<<"no_files">>], Context);
validate(#cb_context{req_verb = <<"post">>, req_files=[{_Filename, FileObj}]}=Context, MediaID, ?BIN_DATA) ->
    case wh_json:get_value([<<"contents">>], FileObj, <<>>) of
        <<>> ->
            crossbar_util:response_invalid_data([<<"empty_file">>], Context);
        _ ->
            lookup_media_by_id(MediaID, Context)
    end.

get(Context, _MediaID, ?BIN_DATA) ->
    Context#cb_context{resp_headers = [{<<"Content-Type">>
                                            ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                       ,{<<"Content-Length">>
                                             ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                       | Context#cb_context.resp_headers]}.

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(#cb_context{req_files=[], resp_status=RespStatus}=Context, _MediaID) ->
    case RespStatus =:= success of
        true -> crossbar_doc:save(Context);
        false -> Context
    end.
post(#cb_context{req_files=[{_, FileObj}|_Files]}=Context, MediaID, ?BIN_DATA) ->
    HeadersJObj = wh_json:get_value(<<"headers">>, FileObj),
    Contents = wh_json:get_value(<<"contents">>, FileObj),

    update_media_binary(MediaID, Contents, Context, HeadersJObj).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(Context, _MediaID) ->
    delete_media(Context).
delete(Context, MediaID, ?BIN_DATA) ->
    delete_media_binary(MediaID, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_media_meta/2 :: (wh_json:json_object(), #cb_context{}) -> #cb_context{}.
create_media_meta(Data, Context) ->
    case wh_json_validator:is_valid(Data, <<"media">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

-spec update_media_binary/4 :: (ne_binary(), ne_binary(), #cb_context{}, wh_json:json_object()) -> #cb_context{}.
update_media_binary(MediaID, Contents, Context, HeadersJObj) ->
    CT = wh_json:get_value(<<"content_type">>, HeadersJObj, <<"application/octet-stream">>),
    Opts = [{headers, [{content_type, wh_util:to_list(CT)}]}],

    lager:debug("file content type: ~s", [CT]),

    case crossbar_doc:save_attachment(MediaID, attachment_name(), Contents, Context, Opts) of
        #cb_context{resp_status=success}=Context1 ->
            lager:debug("saved attachement successfully"),
            #cb_context{doc=Doc} = crossbar_doc:load(MediaID, Context),
            Doc1 = wh_json:set_values([{<<"content_type">>, CT}
                                       ,{<<"content_length">>, wh_json:get_integer_value(<<"content_length">>, HeadersJObj)}
                                      ], Doc),
            crossbar_doc:save(Context1#cb_context{doc=Doc1});
        C ->
            lager:debug("failed to save attachment"),
            C
    end.

%% GET /media
-spec lookup_media/1 :: (Context :: #cb_context{}) -> #cb_context{}.
lookup_media(Context) ->
    case crossbar_doc:load_view(<<"media/crossbar_listing">>, [], Context) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
            Resp = [ wh_json:get_value(<<"value">>, ViewObj) || ViewObj <- Doc ],
            crossbar_util:response(Resp, Context1);
        C -> C
    end.

%% GET/POST/DELETE /media/MediaID
-spec get_media_doc/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
get_media_doc(MediaID, Context) ->
    crossbar_doc:load(MediaID, Context).

%% GET/DELETE /media/MediaID/raw
get_media_binary(MediaID, Context) ->
    case crossbar_doc:load(MediaID, Context) of
        #cb_context{resp_status=success, doc=MediaMeta} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], MediaMeta)) of
                [] -> crossbar_util:response_bad_identifier(MediaID, Context);
                [AttachmentID|_] ->
                    Context1 = crossbar_doc:load_attachment(MediaMeta, AttachmentID, Context),
                    [Type, SubType] = binary:split(content_type_of(MediaMeta), <<"/">>),
                    lager:debug("getting media of type ~s/~s", [Type, SubType]),
                    Context1#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
            end;
        Context1 -> Context1
    end.

%% check for existence of media by name
-spec lookup_media_by_name/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
lookup_media_by_name(MediaName, Context) ->
    crossbar_doc:load_view(<<"media/listing_by_name">>, [{<<"key">>, MediaName}], Context).

%% check for existence of media by id
-spec(lookup_media_by_id/2 :: (MediaID :: binary(), Context :: #cb_context{}) -> #cb_context{}).
lookup_media_by_id(MediaID, Context) ->
    crossbar_doc:load_view(<<"media/crossbar_listing">>, [{<<"key">>, MediaID}], Context).

delete_media(Context) ->
    crossbar_doc:delete(Context).

delete_media_binary(MediaID, Context) ->
    case crossbar_doc:load(MediaID, Context) of
        #cb_context{resp_status=success, doc=MediaMeta} ->
            case wh_json:get_value([<<"_attachments">>, 1], MediaMeta) of
                undefined -> crossbar_util:response_bad_identifier(MediaID, Context);
                AttachMeta ->
                    [AttachmentID] = wh_json:get_keys(AttachMeta),
                    crossbar_doc:delete_attachment(MediaID, AttachmentID, Context)
            end;
        Context1 -> Context1
    end.

-spec attachment_name/0 :: () -> ne_binary().
attachment_name() ->
    wh_util:to_hex_binary(crypto:rand_bytes(16)).

-spec content_type_of/1 :: (wh_json:json_object()) -> ne_binary().
content_type_of(JObj) ->
    case wh_json:get_value(<<"content_type">>, JObj) of
        undefined ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj, wh_json:new())) of
                [] -> <<"application/octect-stream">>;
                [Key|_] -> wh_json:get_value([Key, <<"content_type">>], JObj, <<"application/octet-stream">>)
            end;
        CT -> CT
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).
