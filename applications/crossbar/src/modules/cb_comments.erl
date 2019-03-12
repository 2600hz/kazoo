%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
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

        ,sort/1
        ]).

-include("crossbar.hrl").

-define(COMMENTS, <<"comments">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_CommentId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /comments => []
%%    /comments/foo => [<<"foo">>]
%%    /comments/foo/bar => [<<"foo">>, <<"bar">>]
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
%% /comments might load a list of skel objects
%% /comments/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    C1 = set_resource(Context),
    {Type, _} = cb_context:fetch(C1, 'resource'),
    validate_comments(C1, Type, cb_context:req_verb(Context)).

-spec validate_comments(cb_context:context(), path_token(), http_method()) ->
                               cb_context:context().
validate_comments(Context, _, ?HTTP_GET) ->
    summary(Context);
validate_comments(Context, _, ?HTTP_PUT) ->
    load_doc(Context);
validate_comments(Context, <<"port_requests">>, ?HTTP_DELETE) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"delete operation on comments is not allowed">>}
            ,{<<"cause">>, <<"comments">>}
            ]),
    cb_context:add_validation_error(<<"comments">>, <<"forbidden">>, Msg, Context);
validate_comments(Context, _, ?HTTP_DELETE) ->
    load_doc(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------`
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    C1 = set_resource(Context),
    {Type, _} = cb_context:fetch(C1, 'resource'),
    validate_comment(C1, Id, Type, cb_context:req_verb(Context)).

-spec validate_comment(cb_context:context(), path_token(), path_token(), http_method()) ->
                              cb_context:context().
validate_comment(Context, _, <<"port_requests">>, _) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"operation on a single comment is not allowed">>}
            ,{<<"cause">>, <<"comments">>}
            ]),
    cb_context:add_validation_error(<<"comments">>, <<"forbidden">>, Msg, Context);
validate_comment(Context, Id, _, ?HTTP_GET) ->
    read(Context, Id);
validate_comment(Context, Id, _, ?HTTP_POST) ->
    check_comment_number(Context, Id);
validate_comment(Context, Id, _, ?HTTP_DELETE) ->
    check_comment_number(Context, Id).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = create(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    Context1 = update(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comment(Context1, Id);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    Context1 = remove(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    Context1 = remove(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc The response has gone out, do some cleanup of your own here.
%% @end
%%------------------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> 'ok'.
finish_request(Context) ->
    Resource = cb_context:fetch(Context, 'resource'),
    Verb = cb_context:req_verb(Context),
    finish_req(Context, Resource, Verb).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_resource(cb_context:context()) -> cb_context:context().
set_resource(Context) ->
    set_resource(Context, cb_context:req_nouns(Context)).

-spec set_resource(cb_context:context(), req_nouns()) -> cb_context:context().
set_resource(Context, [{?COMMENTS, _}, Data | _]) ->
    cb_context:store(Context, 'resource', Data).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = load_doc(Context),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comments(Context1);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 = check_comment_number(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> only_return_comment(Context1, Id);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    create(Context, cb_context:fetch(Context, 'resource')).

-spec create(cb_context:context(), {kz_term:ne_binary(), kz_term:ne_binaries()}) -> cb_context:context().
create(Context, {<<"port_requests">>, _}) ->
    NewComments = cb_context:fetch(Context, 'req_comments', []),
    case phonebook:maybe_add_comment(Context, NewComments) of
        {'ok', _} ->
            crossbar_doc:save(Context);
        {'error', _} ->
            cb_context:add_system_error('datastore_fault', <<"unable to submit comment to carrier">>, Context)
    end;
create(Context, _Resource) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    ReqData = cb_context:req_data(Context),
    NewComments = kz_json:get_value(?COMMENTS, ReqData, []),
    Doc1 = kz_json:set_value(?COMMENTS, sort(Comments ++ NewComments), Doc),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
update(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),

    Comment = cb_context:req_data(Context),
    {Head, Tail} = lists:split(Number, Comments),
    Head1 = lists:delete(lists:last(Head), Head),

    Doc1 =
        kz_json:set_value(?COMMENTS
                         ,sort(lists:append([Head1, [Comment], Tail]))
                         ,Doc
                         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec remove(cb_context:context()) -> cb_context:context().
remove(Context) ->
    Doc = kz_json:set_value(?COMMENTS, [], cb_context:doc(Context)),
    crossbar_doc:save(cb_context:set_doc(Context, Doc)).

-spec remove(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
remove(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),
    Comment = lists:nth(Number, Comments),
    Doc1 =
        kz_json:set_value(?COMMENTS
                         ,lists:delete(Comment, Comments)
                         ,Doc
                         ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc1)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec finish_req(cb_context:context(), path_token() | 'undefined', http_method()) -> 'ok'.
finish_req(_, 'undefined', _) ->
    'ok';
finish_req(Context, {<<"port_requests">>, [PortReqId]}, ?HTTP_PUT) ->
    cb_port_requests:send_port_comment_notifications(Context, PortReqId);
finish_req(Context, {<<"port_requests">>, [PortReqId]}, ?HTTP_POST) ->
    cb_port_requests:send_port_comment_notifications(Context, PortReqId);
finish_req(_Context, _Type, _Verb) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_comment_number(cb_context:context(), kz_term:ne_binary()) ->
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_doc(cb_context:context()) -> cb_context:context().
load_doc(Context) ->
    {Type, Id} = cb_context:fetch(Context, 'resource'),
    load_doc(Context, Type, Id).

-spec load_doc(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
                      cb_context:context().
load_doc(Context0, <<"port_requests">>, [Id]) ->
    Comments = kzd_port_requests:comments(cb_context:req_data(Context0), []),
    Context1 = cb_context:store(Context0, 'req_comments', Comments),
    Context2 = cb_port_requests:load_port_request(cb_port_requests:set_port_authority(Context1), Id),
    case cb_context:resp_status(Context2) =:= 'success' of
        'true' ->
            cb_port_requests:validate_port_comments(Context2, fun kz_term:identity/1);
        'false' ->
            Context2
    end;
load_doc(Context, _Type, [Id]) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION_ANY);
load_doc(Context, _Type, _) ->
    cb_context:add_system_error('bad_identifier', Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec only_return_comments(cb_context:context()) -> cb_context:context().
only_return_comments(Context) ->
    Comments = get_comments(Context),
    cb_context:set_resp_data(Context
                            ,kz_json:from_list([{?COMMENTS, Comments}])
                            ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec only_return_comment(cb_context:context(), kz_term:ne_binary()) ->
                                 cb_context:context().
only_return_comment(Context, Id) ->
    Doc = cb_context:doc(Context),
    Comments = kz_json:get_value(?COMMENTS, Doc, []),
    Number = id_to_number(Id),
    cb_context:set_resp_data(Context
                            ,lists:nth(Number, Comments)
                            ).

get_comments(Context) ->
    {Type, _} = cb_context:fetch(Context, 'resource'),
    get_comments(Context, Type).

get_comments(Context, <<"port_requests">>) ->
    Doc = cb_context:doc(Context),
    kzd_port_requests:comments(cb_port_requests:filter_private_comments(Context, Doc), []);
get_comments(Context, _) ->
    kz_json:get_value(?COMMENTS, cb_context:doc(Context), []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id_to_number(kz_term:ne_binary()) -> pos_integer().
id_to_number(Id) -> kz_term:to_integer(Id) + 1.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sort(kz_json:objects()) -> boolean().
sort(Comments) ->
    lists:sort(fun sort_fun/2, Comments).

-spec sort_fun(kz_json:object(), kz_json:object()) -> boolean().
sort_fun(CommentA, CommentB) ->
    kz_json:get_integer_value(<<"timestamp">>, CommentA, 0) =< kz_json:get_integer_value(<<"timestamp">>, CommentB, 0).
