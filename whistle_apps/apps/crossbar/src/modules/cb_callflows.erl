%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Callflow gen server for CRUD
%%%
%%% @end
%%% @contributors
%%%   Vladimir Darmin
%%%   James Aimonetti
%%%----------------------------------------------------------------------------
-module(cb_callflows).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CALLFLOWS_LIST, <<"callflows/listing_by_id">>).
-define(CB_LIST, <<"callflows/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.callflows">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.callflows">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.callflows">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.callflows">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.callflows">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.callflows">>, ?MODULE, delete).

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
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_MediaID) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> boolean().
-spec resource_exists/1 :: (path_token()) -> boolean().
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_callflow_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_callflow(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_callflow(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_callflow(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_callflow(DocId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _DocId) ->
    case crossbar_doc:save(Context) of
        #cb_context{account_id=AccountId, doc=JObj, resp_status=success}=C ->
            spawn(fun() -> 
                          [wh_number_manager:reconcile_number(Number, AccountId)
                           || Number <- wh_json:get_value(<<"numbers">>, JObj, [])]
                  end),
            C;
        Else ->
            Else
    end.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    case crossbar_doc:save(Context) of
        #cb_context{account_id=AccountId, doc=JObj, resp_status=success}=C ->
            spawn(fun() -> 
                          [wh_number_manager:reconcile_number(Number, AccountId)
                           || Number <- wh_json:get_value(<<"numbers">>, JObj, [])]
                  end),
            C;
        Else ->
            Else
    end.

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _DocId) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_callflow_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_callflow_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new callflow document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_callflow/1 :: (#cb_context{}) -> #cb_context{}.
create_callflow(#cb_context{req_data=Data}=Context) ->
    try [wnm_util:to_e164(Number) || Number <- wh_json:get_value(<<"numbers">>, Data, [])] of
        Numbers ->
            Data1 = wh_json:set_value(<<"numbers">>, Numbers, Data),
            case wh_json_validator:is_valid(Data1, <<"callflows">>) of
                {fail, Errors} ->
                    crossbar_util:response_invalid_data(Errors, Context);
                {pass, JObj} ->
                    Context#cb_context{
                      doc=wh_json:set_value(<<"pvt_type">>, <<"callflow">>, JObj)
                      ,resp_status=success
                     }
            end
    catch
        _:_ ->
            Errs = [wh_json:set_value([<<"numbers">>, <<"type">>], <<"Value is not of type array">>, wh_json:new())],
            crossbar_util:response_invalid_data(Errs, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a callflow document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_callflow/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_callflow(DocId, Context) ->
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc, resp_data=Data, db_name=Db}=Context1 ->
            Meta = get_metadata(wh_json:get_value(<<"flow">>, Doc), Db, wh_json:new()),
            Context1#cb_context{resp_data=wh_json:set_value(<<"metadata">>, Meta, Data)};
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing callflow document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_callflow/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_callflow(DocId, #cb_context{req_data=Data}=Context) ->
    Numbers = [wnm_util:to_e164(Number) || Number <- wh_json:get_value(<<"numbers">>, Data, [])],
    Data1 = wh_json:set_value(<<"numbers">>, Numbers, Data),
    case wh_json_validator:is_valid(Data1, <<"callflows">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% collect addional informat about the objects referenced in the flow
%% @end
%%--------------------------------------------------------------------
-spec get_metadata/3 :: ('undefined' | wh_json:json_object(), ne_binary(), wh_json:json_object()) -> wh_json:json_object().
get_metadata(undefined, _, JObj) ->
    JObj;
get_metadata(Flow, Db, JObj) ->
    JObj1 = case wh_json:get_value([<<"data">>, <<"id">>], Flow) of
                %% this node has no id, dont change the metadata
                undefined -> JObj;
                %% node has an id, try to update the metadata
                Id -> create_metadata(Db, Id, JObj)
            end,
    case wh_json:get_value(<<"children">>, Flow) of
        undefined -> JObj1;
        Children ->
            %% iterate through each child, collecting metadata on the
            %% branch name (things like temporal routes)
            lists:foldr(fun({K, Child}, J) ->
                                get_metadata(Child, Db, create_metadata(Db, K, J))
                        end, JObj1, wh_json:to_proplist(Children))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the metadata json object, an ID and a db find the document
%% and add the fields to the metadata.  However, skip if the ID already
%% exists in metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_metadata/3 :: (Db, Id, JObj) -> wh_json:json_object() when
      Db :: binary(),
      Id :: binary(),
      JObj :: wh_json:json_object().
create_metadata(Db, Id, JObj) ->
    case wh_json:get_value(Id, JObj) =:= undefined
        andalso couch_mgr:open_doc(Db, Id) of
        false  ->
            %% the id already exists in the metadata
            JObj;
        {ok, Doc} ->
            %% the id was found in the db
            wh_json:set_value(Id, create_metadata(Doc), JObj);
        _ ->
            %% eh, whatevs
            JObj
    end.

-spec create_metadata/1 :: (Doc) -> wh_json:json_object() when
      Doc :: wh_json:json_object().
create_metadata(Doc) ->
    %% simple funciton for setting the same key in one json object
    %% with the value of that key in another, unless it doesnt exist
    Metadata = fun(<<"name">> = K, D, J) ->
                       case wh_json:get_value(<<"pvt_type">>, D) of
                           <<"user">> ->
                               Name = <<(wh_json:get_binary_value(<<"first_name">>, D, <<>>))/binary
                                        ," "
                                        ,(wh_json:get_binary_value(<<"last_name">>, D, <<>>))/binary>>,
                               case Name of
                                   <<>> -> J;
                                   _ -> wh_json:set_value(<<"name">>, Name, J)
                               end;
                           _ ->
                               case wh_json:get_value(K, D) of
                                   undefined -> J;
                                   V -> wh_json:set_value(K, V, J)
                               end
                       end;
                  (K, D, J) ->
                       case wh_json:get_value(K, D) of
                           undefined -> J;
                           V -> wh_json:set_value(K, V, J)
                       end
               end,
    %% list of keys to extract from documents and set on the metadata
    Funs = [fun(D, J) -> Metadata(<<"name">>, D, J) end,
            fun(D, J) -> Metadata(<<"numbers">>, D, J) end,
            fun(D, J) -> Metadata(<<"pvt_type">>, D, J) end],
    %% do it
    lists:foldl(fun(Fun, JObj) ->
                         Fun(Doc, JObj)
                end, wh_json:new(), Funs).
