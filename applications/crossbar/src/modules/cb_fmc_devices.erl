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
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', Doc} ->
            AccountId = cb_context:account_id(Context),
            case wh_json:get_value(<<"account_id">>, Doc) of
                AccountId ->
                    Context1 = cb_context:set_doc(Context, Doc),
                    read(Context1, Id);
                _ ->
                    cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
            end;
        _ ->
            cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
    end;
validate_fmc_device(Context, Id, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"fmc_device">>, Context, OnSuccess);
validate_fmc_device(Context, Id, ?HTTP_DELETE) ->
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', SavedDoc} ->
            AccountId = cb_context:account_id(Context),
            case wh_json:get_value(<<"account_id">>, SavedDoc) of
                AccountId ->
                    Context1 = cb_context:set_doc(Context, SavedDoc),
                    delete(Context1, Id);
                _ ->
                    cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
            end;
        _ ->
            cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    {'ok', JObjs} = couch_mgr:get_all_results(?WH_FMC_DB, ?CB_LIST),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    Doc1 = [JObj || JObj <- Doc, wh_json:get_value(<<"account_id">>, JObj) =:= cb_context:account_id(Context)],
    Doc2 = [wh_json:delete_keys([<<"a_number">>, <<"account_id">>], JObj) || JObj <- Doc1],
    Context1 = cb_context:set_doc(Context, Doc2),
    Context2 = cb_context:set_resp_data(Context1, Doc2),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), path_token()) -> cb_context:context().
read(Context, Id) ->
    {'ok', Doc} = couch_mgr:open_cache_doc(?WH_FMC_DB, Id),
    Doc1 = wh_doc:public_fields(Doc),
    Doc2 = wh_json:delete_keys([<<"a_number">>, <<"account_id">>], Doc1),
    Context1 = cb_context:set_doc(Context, Doc2),
    Context2 = cb_context:set_resp_data(Context1, Doc2),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fmc_device">>}
                              ,{<<"account_id">>, cb_context:account_id(Context)}]
                              ,cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, Doc),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId),
    ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
    Doc1 = wh_json:set_value(<<"a_number">>, ANumber, Doc),
    {'ok', ResultDoc} = couch_mgr:save_doc(?WH_FMC_DB, Doc1),
    ResultDoc1 = wh_doc:public_fields(ResultDoc),
    Context1 = cb_context:set_doc(Context, ResultDoc1),
    cb_context:set_resp_data(Context1, ResultDoc1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    Doc1 = wh_json:set_values([{<<"_id">>, Id}
        ,{<<"pvt_type">>, <<"fmc_device">>}
        ,{<<"account_id">>, AccountId}]
        ,cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, Doc1),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId),
    ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
    Doc2 = wh_json:set_value(<<"a_number">>, ANumber, Doc1),
    {'ok', ResultDoc} = couch_mgr:save_doc(?WH_FMC_DB, Doc2),
    ResultDoc1 = wh_doc:public_fields(ResultDoc),
    Context1 = cb_context:set_doc(Context, ResultDoc1),
    cb_context:set_resp_data(Context1, ResultDoc1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    {'ok', _} = couch_mgr:del_doc(?WH_FMC_DB, Id),
    cb_context:set_resp_status(Context, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    FMCValue = wh_json:get_value(<<"x_fmc_value">>, cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, cb_context:doc(Context)),
    case couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId) of
        {'ok', DeviceDoc} ->
            Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc_device">>, cb_context:doc(Context)),
            DeviceType = wh_json:get_value(<<"device_type">>, DeviceDoc),
            Number = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
            case DeviceType of
                <<"cellphone">> ->
                    case is_existing_fmc(Number, FMCValue, 'undefined') of
                        'true' ->
                            cb_context:add_validation_error(<<"fmc">>, <<"unique">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this A-Number and FMC Value already exists">>}]), Context);
                        'false' ->
                            cb_context:set_doc(Context, Doc)
                    end;
                _ ->
                    cb_context:add_validation_error(<<"fmc">>, <<"invalid">>, wh_json:from_list([{<<"message">>, <<"Invalid device type">>}]), Context)
            end;
        {'error', _} ->
            cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"Device with this ID wasn't found">>}]), Context)
    end;
on_successful_validation(Id, Context) ->
    FMCValue = wh_json:get_value(<<"x_fmc_value">>, cb_context:doc(Context)),
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', Doc} ->
            case wh_doc:is_soft_deleted(Doc) of
                'true' ->
                    cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"Device with this ID wasn't found">>}]), Context);
                'false' ->
                    AccountId = cb_context:account_id(Context),
                    case wh_json:get_value(<<"account_id">>, Doc) of
                        AccountId ->
                            DeviceId = wh_json:get_value(<<"device_id">>, Doc),
                            {'ok', DeviceDoc} = couch_mgr:open_doc(cb_context:account_db(Context), DeviceId),
                            case wh_json:get_value(<<"device_type">>, DeviceDoc) of
                                <<"cellphone">> ->
                                    Number = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
                                    case is_existing_fmc(Number, FMCValue, Id) of
                                        'true' ->
                                            cb_context:add_validation_error(<<"fmc">>, <<"unique">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this A-Number and FMC Value already exists">>}]), Context);
                                        'false' ->
                                            PrivJObj = wh_json:private_fields(Doc),
                                            NewDoc = wh_json:merge_jobjs(PrivJObj, cb_context:doc(Context)),
                                            cb_context:set_doc(Context, NewDoc)
                                    end;
                                _ ->
                                    cb_context:add_validation_error(<<"fmc">>, <<"invalid">>, wh_json:from_list([{<<"message">>, <<"Invalid device type">>}]), Context)
                            end;
                        _ ->
                            cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
                    end
            end;
        _ ->
            cb_context:add_validation_error(<<"fmc">>, <<"not_found">>, wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}]), Context)
    end.

is_existing_fmc(Number, FMCValue, 'undefined') ->
    {'ok', JObjs} = couch_mgr:get_all_results(?WH_FMC_DB, ?CB_LIST),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    Doc1 = [{wh_json:get_value(<<"a_number">>, JObj), wh_json:get_value(<<"x_fmc_value">>, JObj)} || JObj <- Doc],
    lists:member({Number, FMCValue}, Doc1);
is_existing_fmc(Number, FMCValue, FMCDocId) ->
    {'ok', JObjs} = couch_mgr:get_all_results(?WH_FMC_DB, ?CB_LIST),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    Doc1 = [{wh_json:get_value(<<"a_number">>, JObj), wh_json:get_value(<<"x_fmc_value">>, JObj)} || JObj <- Doc, wh_json:get_value(<<"id">>, JObj) =/= FMCDocId],
    lists:member({Number, FMCValue}, Doc1).
