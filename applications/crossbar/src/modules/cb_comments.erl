%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_comments).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/1 ,delete/2
         ,finish_request/1
        ]).

-include("crossbar.hrl").

-define(COMMENTS, <<"comments">>).

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
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.comments">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.comments">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.comments">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.comments">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.comments">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.comments">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.finish_request.*.comments">>, ?MODULE, 'finish_request').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(_) -> 'false'.

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
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /comments => []
%%    /comments/foo => [<<"foo">>]
%%    /comments/foo/bar => [<<"foo">>, <<"bar">>]
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
%% /comments mights load a list of skel objects
%% /comments/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_comments(get_ressource(Context), cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_comment(get_ressource(Context), Id, cb_context:req_verb(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = create(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    Context1 = update(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comment(Context1, Id);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context) ->
    Context1 = remove(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

delete(Context, Id) ->
    Context1 = remove(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% The response has gone out, do some cleanup of your own here.
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> 'ok'.
finish_request(Context) ->
    {Type, _} = cb_context:fetch(Context, 'ressource'),
    Verb = cb_context:req_verb(Context),
    finish_req(Context, Type, Verb).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_ressource(cb_context:context()) -> cb_context:context().
-spec get_ressource(cb_context:context(), req_nouns()) -> cb_context:context().
get_ressource(Context) ->
    get_ressource(Context, cb_context:req_nouns(Context)).

get_ressource(Context, [{?COMMENTS, _}, Data | _]) ->
    cb_context:store(Context, 'ressource', Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_comments(cb_context:context(), http_method()) ->
                               cb_context:context().
validate_comments(Context, ?HTTP_GET) ->
    summary(Context);
validate_comments(Context, ?HTTP_PUT) ->
    load_doc(Context);
validate_comments(Context, ?HTTP_DELETE) ->
    load_doc(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------`
-spec validate_comment(cb_context:context(), path_token(), http_method()) ->
                              cb_context:context().
validate_comment(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_comment(Context, Id, ?HTTP_POST) ->
    check_comment_number(Context, Id);
validate_comment(Context, Id, ?HTTP_DELETE) ->
    check_comment_number(Context, Id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = load_doc(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 = check_comment_number(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comment(Context1, Id);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),

    ReqData = cb_context:req_data(Context),
    NewComments = kz_json:get_value(?COMMENTS, ReqData, []),

    Doc1 =
        kz_json:set_value(
          ?COMMENTS
          ,lists:append(Comments, NewComments)
          ,Doc
         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),

    Comment = cb_context:req_data(Context),
    {Head, Tail} = lists:split(Number, Comments),
    Head1 = lists:delete(lists:last(Head), Head),

    Doc1 =
        kz_json:set_value(
          ?COMMENTS
          ,lists:append([Head1, [Comment], Tail])
          ,Doc
         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove(cb_context:context()) -> cb_context:context().
-spec remove(cb_context:context(), ne_binary()) -> cb_context:context().
remove(Context) ->
    Doc =
        kz_json:set_value(
          ?COMMENTS
          ,[]
          ,cb_context:doc(Context)
         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc)).

remove(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),
    Comment = lists:nth(Number, Comments),
    Doc1 =
        kz_json:set_value(
          ?COMMENTS
          ,lists:delete(Comment, Comments)
          ,Doc
         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec finish_req(cb_context:context(), path_token(), http_method()) -> 'ok'.
finish_req(Context, <<"port_requests">>, ?HTTP_PUT) ->
   send_port_comment_notification(Context);
finish_req(Context, <<"port_requests">>, ?HTTP_POST) ->
    send_port_comment_notification(Context);
finish_req(_Context, _Type, _Verb) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_comment_number(cb_context:context(), ne_binary()) ->
                                  cb_context:context().
check_comment_number(Context, Id) ->
    Context1 = load_doc(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Doc = cb_context:doc(Context1),
            Comments = kz_json:get_value(?COMMENTS, Doc, []),
            Number = id_to_number(Id),
            case erlang:length(Comments) of
                Length when Length < Number ->
                    cb_context:add_system_error('bad_identifier', Context1);
                _ -> Context1
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_doc(cb_context:context()) ->
                      cb_context:context().
-spec load_doc(cb_context:context(), ne_binary(), ne_binaries()) ->
                      cb_context:context().
load_doc(Context) ->
    {Type, Id} = cb_context:fetch(Context, 'ressource'),
    load_doc(Context, Type, Id).

load_doc(Context, <<"port_requests">>, [Id]) ->
    crossbar_doc:load(Id
                      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                      ,?TYPE_CHECK_OPTION(<<"port_request">>));
load_doc(Context, _Type, [Id]) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION_ANY);
load_doc(Context, _Type, _) ->
    cb_context:add_system_error('bad_identifier', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec only_return_comments(cb_context:context()) -> cb_context:context().
only_return_comments(Context) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    cb_context:set_resp_data(
      Context
      ,kz_json:from_list([{?COMMENTS, Comments}])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec only_return_comment(cb_context:context(), ne_binary()) ->
                                 cb_context:context().
only_return_comment(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),
    cb_context:set_resp_data(
      Context
      ,lists:nth(Number, Comments)
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id_to_number(ne_binary()) -> pos_integer().
id_to_number(Id) -> kz_term:to_integer(Id) + 1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_comment_notification(cb_context:context()) -> 'ok'.
-spec send_port_comment_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_comment_notification(Context) ->
    ReqNouns = cb_context:req_nouns(Context),
    [PortReqId] = props:get_value(<<"port_requests">>, ReqNouns),
    send_port_comment_notification(Context, PortReqId).

send_port_comment_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           ,{<<"Version">>, cb_context:api_version(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_comment/1).
