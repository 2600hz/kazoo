%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Acess lists
%%% /accounts/{account_id}/access_lists - manip account's access lists
%%% /accounts/{account_id}/devices/{device_id}/access_lists - manip user's access lists
%%%
%%% @end
%%% @contributors:
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cb_access_lists).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
        ]).

-include("crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.access_lists">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.access_lists">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.access_lists">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.access_lists">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.access_lists">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /access_lists => []
%%    /access_lists/foo => [<<"foo">>]
%%    /access_lists/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /access_lists mights load a list of metaflow objects
%% /access_lists/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_acls(Context, cb_context:req_verb(Context)).

-spec validate_acls(cb_context:context(), http_method()) -> cb_context:context().
validate_acls(Context, ?HTTP_GET) ->
    validate_get_acls(thing_doc(Context));
validate_acls(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"access_lists">>, Context, fun validate_set_acls/1);
validate_acls(Context, ?HTTP_DELETE) ->
    validate_delete_acls(thing_doc(Context)).

-spec thing_doc(cb_context:context()) -> cb_context:context().
-spec thing_doc(cb_context:context(), ne_binary(), api(binary())) -> cb_context:context().
thing_doc(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"access_lists">>, []}, {<<"accounts">>, [AccountId]} | _] ->
            lager:debug("loading access lists from account: '~s'", [AccountId]),
            thing_doc(Context, AccountId, kz_account:type());
        [{<<"access_lists">>, []}, {<<"devices">>, [DeviceId]} | _] ->
            lager:debug("loading access lists from device: '~s'", [DeviceId]),
            thing_doc(Context, DeviceId, kz_device:type());
        _Nouns ->
            cb_context:add_system_error(
              'faulty_request'
              ,<<"access lists not supported on this URI path">>
              ,Context
             )
    end.

thing_doc(Context, ThingId, Type) ->
    Context1 = crossbar_doc:load(ThingId, Context, ?TYPE_CHECK_OPTION(Type)),
    case cb_context:resp_status(Context1) of
        'success' -> Context1;
        _Status ->
            lager:debug("failed to load thing ~s", [ThingId]),
            thing_id_not_found(Context1, ThingId)
    end.

-spec thing_id_not_found(cb_context:context(), ne_binary()) -> cb_context:context().
thing_id_not_found(Context, ThingId) ->
    cb_context:add_system_error(
      'bad_identifier'
      ,kz_json:from_list([{<<"cause">>, <<"Identifier was not found">>}
                          ,{<<"details">>, ThingId}
                         ])
      ,Context
     ).

-spec validate_get_acls(cb_context:context()) -> cb_context:context().
-spec validate_get_acls(cb_context:context(), kz_json:object()) -> cb_context:context().
validate_get_acls(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> validate_get_acls(Context, cb_context:doc(Context));
        _Status -> Context
    end.

validate_get_acls(Context, Doc) ->
    AccessLists = kz_json:get_value(<<"access_lists">>, Doc, kz_json:new()),
    crossbar_util:response(AccessLists, Context).

-spec validate_delete_acls(cb_context:context()) -> cb_context:context().
-spec validate_delete_acls(cb_context:context(), api(kz_json:object())) -> cb_context:context().
validate_delete_acls(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> validate_delete_acls(Context, cb_context:doc(Context));
        _Status -> Context
    end.

validate_delete_acls(Context, Doc) ->
    crossbar_util:response(kz_json:new()
                           ,cb_context:set_doc(Context
                                               ,kz_json:delete_key(<<"access_lists">>, Doc)
                                              )).

-spec validate_set_acls(cb_context:context()) ->
                               cb_context:context().
-spec validate_set_acls(cb_context:context(), kz_json:object(), api(kz_json:object())) ->
                               cb_context:context().
validate_set_acls(Context) ->
    lager:debug("access lists data is valid, setting on thing"),
    validate_set_acls(thing_doc(Context), cb_context:doc(Context)).

validate_set_acls(Context, AccessLists) ->
    case cb_context:resp_status(Context) of
        'success' -> validate_set_acls(Context, AccessLists, cb_context:doc(Context));
        _Status -> Context
    end.

validate_set_acls(Context, AccessLists, Doc) ->
    Doc1 = kz_json:set_value(<<"access_lists">>, AccessLists, Doc),

    crossbar_util:response(AccessLists
                           ,cb_context:set_doc(Context, Doc1)
                          ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    after_post(crossbar_doc:save(Context)).

-spec after_post(cb_context:context()) -> cb_context:context().
-spec after_post(cb_context:context(), crossbar_status()) -> cb_context:context().
after_post(Context) ->
    after_post(Context, cb_context:resp_status(Context)).

after_post(Context, 'success') ->
    crossbar_util:response(kz_json:get_value(<<"access_lists">>, cb_context:doc(Context))
                           ,Context
                          );
after_post(Context, _RespStatus) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    after_delete(crossbar_doc:save(Context)).

-spec after_delete(cb_context:context()) -> cb_context:context().
-spec after_delete(cb_context:context(), crossbar_status()) -> cb_context:context().
after_delete(Context) ->
    after_delete(Context, cb_context:resp_status(Context)).

after_delete(Context, 'success') ->
    crossbar_util:response(kz_json:new(), Context);
after_delete(Context, _RespStatus) ->
    Context.
