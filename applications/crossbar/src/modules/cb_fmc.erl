%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cb_fmc).

-export([init/0
    ,allowed_methods/0, allowed_methods/1, allowed_methods/2
    ,resource_exists/0, resource_exists/1, resource_exists/2
    ,validate/1, validate/2, validate/3
    ,read/1
    ,put/1, put/2
    ,post/1, post/3
    ,delete/2
]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.fmc">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.fmc">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.fmc">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.fmc">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.fmc">>, ?MODULE, 'post').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_, _) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% Expected: /fmc
%%           /fmc/numbers
%%           /fmc/numbers/${number}
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /fmc mights interoperate with FMC configuration
%% /fmc/numbers might load all FMC numbers
%% /fmc/numbers/${number} might interoperate with this FMC number
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_config(Context, cb_context:req_verb(Context)).
validate(Context, <<"numbers">>) ->
    validate_numbers(Context, cb_context:req_verb(Context)).
validate(Context, <<"numbers">>, Number) ->
    validate_number(Context, Number, cb_context:req_verb(Context)).

-spec validate_config(cb_context:context(), http_method()) -> cb_context:context().
validate_config(Context, ?HTTP_GET) ->
    read(Context);
validate_config(Context, ?HTTP_PUT) ->
    OnSuccess = fun(C) -> on_successful_validation(<<"config">>, C) end,
    cb_context:validate_request_data(<<"fmc">>, Context, OnSuccess);
validate_config(Context, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_validation(<<"config">>, C) end,
    cb_context:validate_request_data(<<"fmc">>, Context, OnSuccess).

-spec validate_numbers(cb_context:context(), http_method()) -> cb_context:context().
validate_numbers(Context, ?HTTP_GET) ->
    read_numbers(Context);
validate_numbers(Context, ?HTTP_PUT) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"fmc_item">>, Context, OnSuccess).

-spec validate_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_number(Context, Id, ?HTTP_GET) ->
    read_number(Context, Id);
validate_number(Context, Id, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"fmc_item">>, Context, OnSuccess);
validate_number(Context, Id, ?HTTP_DELETE) ->
    delete(Context, Id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    lager:debug("read/1: ~p", [Context]),
    {'ok', Doc} = couch_mgr:open_doc(?FMC_DB, <<"config">>),
    Doc1 = wh_doc:public_fields(Doc),
    Context1 = cb_context:set_doc(Context, Doc1),
    Context2 = cb_context:set_resp_data(Context1, Doc1),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    lager:debug("put/1: ~p", [Context]),
    Doc = wh_json:set_values([{<<"_id">>, <<"config">>}, {<<"pvt_type">>, <<"config">>}], cb_context:doc(Context)),
    {'ok', _} = couch_mgr:save_doc(?FMC_DB, Doc),
    cb_context:set_doc(Context, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    lager:debug("post/1: ~p", [Context]),
    Doc = cb_context:doc(Context),
    {'ok', _Doc} = couch_mgr:save_doc(?FMC_DB, Doc),
    cb_context:set_doc(Context, Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read_numbers(cb_context:context()) -> cb_context:context().
read_numbers(Context) ->
    lager:debug("read_numbers/1: ~p", [Context]),
    {'ok', JObjs} = couch_mgr:get_all_results(?FMC_DB, <<"fmc/numbers_listing">>),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    Context1 = cb_context:set_doc(Context, Doc),
    Context2 = cb_context:set_resp_data(Context1, Doc),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, <<"numbers">>) ->
    lager:debug("put/2 from numbers: ~p", [Context]),
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"number">>}], cb_context:doc(Context)),
    {'ok', _} = couch_mgr:save_doc(?FMC_DB, Doc),
    cb_context:set_doc(Context, Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read_number(cb_context:context(), path_token()) -> cb_context:context().
read_number(Context, Id) ->
    lager:debug("read_number/2: ~p", [Context]),
    {'ok', Doc} = couch_mgr:open_doc(?FMC_DB, Id),
    Doc1 = wh_doc:public_fields(Doc),
    Context1 = cb_context:set_doc(Context, Doc1),
    Context2 = cb_context:set_resp_data(Context1, Doc1),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, <<"numbers">>, Id) ->
    lager:debug("post/2 from numbers: ~p", [Context]),
    Doc = wh_json:set_value(<<"_id">>, Id, cb_context:doc(Context)),
    {'ok', _} = couch_mgr:save_doc(?FMC_DB, Doc),
    cb_context:set_doc(Context, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    lager:debug("delete/2: ~p", [Context]),
    {'ok', _} = couch_mgr:del_doc(?FMC_DB, Id),
    cb_context:set_resp_status(Context, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc">>, cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successful_validation(Id, Context) ->
    case couch_mgr:open_doc(?FMC_DB, Id) of
        {'ok', Doc} ->
            PrivJObj = wh_json:private_fields(Doc),
            NewDoc = wh_json:merge_jobjs(PrivJObj, cb_context:doc(Context)),
            cb_context:set_doc(Context, NewDoc);
        {'error', 'not_found'} ->
            Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc">>, cb_context:doc(Context)),
            cb_context:set_doc(Context, Doc)
    end.
