%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cb_fmc_devices).

-export([init/0
    ,allowed_methods/0, allowed_methods/1
    ,resource_exists/0, resource_exists/1
    ,validate/1, validate/2
    ,read/1, read/2
    ,put/1
    ,post/2
    ,delete/2
]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"fmc_devices/crossbar_listing">>).
-define(FMC_DB, <<"fmc">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.fmc_devices">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.fmc_devices">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.fmc_devices">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.fmc_devices">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.fmc_devices">>, ?MODULE, 'post').

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
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% Expected: /fmc_devices
%%           /fmc_devices/${fmc_device_id}
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
%% /fmc_devices might load all FMC devices
%% /fmc_devices/${fmc_device_id} might interoperate with this FMC devices
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_fmc_devices(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_fmc_device(Context, Id, cb_context:req_verb(Context)).

-spec validate_fmc_devices(cb_context:context(), http_method()) -> cb_context:context().
validate_fmc_devices(Context, ?HTTP_GET) ->
    read(Context);
validate_fmc_devices(Context, ?HTTP_PUT) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"fmc_device">>, Context, OnSuccess).

-spec validate_fmc_device(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_fmc_device(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_fmc_device(Context, Id, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"fmc_device">>, Context, OnSuccess);
validate_fmc_device(Context, Id, ?HTTP_DELETE) ->
    delete(Context, Id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    lager:debug("read/1: Context is ~p", [Context]),
    {'ok', JObjs} = couch_mgr:get_all_results(?FMC_DB, <<"fmc_devices/crossbar_listing">>),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    Doc1 = [JObj || JObj <- Doc, wh_json:get_value(<<"account_id">>, JObj) =:= cb_context:account_id(Context)],
    lager:debug("read/1: Doc1 is ~p", [Doc1]),
    Context1 = cb_context:set_doc(Context, Doc1),
    Context2 = cb_context:set_resp_data(Context1, Doc1),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), path_token()) -> cb_context:context().
read(Context, Id) ->
    lager:debug("read/2: Context is ~p", [Context]),
    lager:debug("read/2: Id is ~p", [Id]),
    {'ok', Doc} = couch_mgr:open_doc(?FMC_DB, Id),
    lager:debug("read/2: Doc is ~p", [Doc]),
    AccountId = cb_context:account_id(Context),
    case wh_json:get_value(<<"account_id">>, Doc) of
        AccountId ->
            lager:debug("read/2: AccountId is ~p", [AccountId]),
            Doc1 = wh_doc:public_fields(Doc),
            Context1 = cb_context:set_doc(Context, Doc1),
            Context2 = cb_context:set_resp_data(Context1, Doc1),
            cb_context:set_resp_status(Context2, 'success');
        _ ->
            cb_context:set_resp_status(Context, 'error')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    lager:debug("put/1: Context is ~p", [Context]),
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fmc_device">>}
                              ,{<<"account_id">>, cb_context:account_id(Context)}]
                              ,cb_context:doc(Context)),
    lager:debug("put/1: Doc is ~p", [Doc]),
    DeviceId = wh_json:get_value(<<"device_id">>, Doc),
    lager:debug("put/1: DeviceId is ~p", [DeviceId]),
    {'ok', DeviceDoc} = couch_mgr:open_doc(cb_context:account_db(Context), DeviceId),
    lager:debug("put/1: DeviceDoc is ~p", [DeviceDoc]),
    case wh_json:get_value(<<"device_type">>, DeviceDoc) of
        <<"cellphone">> ->
            ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
            Doc1 = wh_json:set_value(<<"a_number">>, ANumber, Doc),
            lager:debug("put/1: Doc1 is ~p", [Doc1]),
            {'ok', _} = couch_mgr:save_doc(?FMC_DB, Doc1),
            cb_context:set_doc(Context, Doc1);
        _ ->
            cb_context:set_resp_status(Context, 'error')
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
    lager:debug("post/2: Context is ~p", [Context]),
    lager:debug("post/2: Id is ~p", [Id]),
    case couch_mgr:open_doc(?FMC_DB, Id) of
        {'ok', Doc} ->
            lager:debug("post/1: Doc is ~p", [Doc]),
            AccountId = cb_context:account_id(Context),
            lager:debug("post/1: AccountId is ~p", [AccountId]),
            case wh_json:get_value(<<"account_id">>, Doc) of
                AccountId ->
                    Doc1 = wh_json:set_values([{<<"_id">>, Id}
                        ,{<<"pvt_type">>, <<"fmc_device">>}
                        ,{<<"account_id">>, AccountId}]
                        ,cb_context:doc(Context)),
                    DeviceId = wh_json:get_value(<<"device_id">>, Doc1),
                    {'ok', DeviceDoc} = couch_mgr:open_doc(cb_context:account_db(Context), DeviceId),
                    lager:debug("post/2: DeviceDoc is ~p", [DeviceDoc]),
                    case wh_json:get_value(<<"device_type">>, DeviceDoc) of
                        <<"cellphone">> ->
                            ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
                            Doc2 = wh_json:set_value(<<"a_number">>, ANumber, Doc1),
                            lager:debug("post/2: Doc2 is ~p", [Doc2]),
                            {'ok', _} = couch_mgr:save_doc(?FMC_DB, Doc2),
                            cb_context:set_doc(Context, Doc2);
                        _ ->
                            cb_context:set_resp_status(Context, 'error')
                    end;
                _ ->
                    cb_context:set_resp_status(Context, 'error')
            end;
        _ ->
            cb_context:set_resp_status(Context, 'error')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    lager:debug("delete/2: Context is ~p", [Context]),
    lager:debug("delete/2: Id is ~p", [Context]),
    {'ok', SavedDoc} = couch_mgr:open_doc(?FMC_DB, Id),
    lager:debug("delete/2: SavedDoc is ~p", [SavedDoc]),
    AccountId = cb_context:account_id(Context),
    lager:debug("delete/2: AccountId is ~p", [AccountId]),
    case wh_json:get_value(<<"account_id">>, SavedDoc) of
        AccountId ->
            {'ok', _} = couch_mgr:del_doc(?FMC_DB, Id),
            lager:debug("delete/2: document deleted"),
            cb_context:set_resp_status(Context, 'success');
        _ ->
            lager:debug("Trying to remove doc of another account."),
            cb_context:set_resp_status(Context, 'error')
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc_device">>, cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successful_validation(Id, Context) ->
    case couch_mgr:open_doc(?FMC_DB, Id) of
        {'ok', Doc} ->
            PrivJObj = wh_json:private_fields(Doc),
            NewDoc = wh_json:merge_jobjs(PrivJObj, cb_context:doc(Context)),
            cb_context:set_doc(Context, NewDoc);
        {'error', 'not_found'} ->
            Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc_device">>, cb_context:doc(Context)),
            cb_context:set_doc(Context, Doc)
    end.
