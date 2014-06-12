%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% Whenever a document is modified (via a PUT, POST, or DELETE) through
%%% Crossbar, set the name of the modifying user and the current time,
%%% allowing modifications to be 'blamed' on an individual user.
%%%
%%% @end
%%% @contributors:
%%%   Sam Metson
%%%-------------------------------------------------------------------
-module(cb_show_modified).

-export([init/0
         ,validate/1%, validate/2, validate/3
        ]).

-include("../crossbar.hrl").

-define(USERNAME_LIST, <<"users/list_by_username">>).
-define(IS_PVT, 'false').

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to
%% (any validate bindings).
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.validate.*">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the username of the user modifying this document,
%% and the current timestamp in UTC.
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    update_modified(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    update_modified(Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context) ->
    update_modified(Context).
validate(Context, _) -> ?MODULE:validate(Context).
validate(Context, _, _) -> ?MODULE:validate(Context).
validate(Context, _, _, _) -> ?MODULE:validate(Context).


-spec update_modified(cb_context:context(), http_method()) -> cb_context:context().
update_modified(Context) ->
    Doc1 = wh_json:set_value(<<"modified_by">>, get_modifying_username(cb_context:auth_doc(Context)), cb_context:doc(Context)),
    Doc2 = wh_json:set_value(<<"modified_time">>, universal_time(), Doc1),
    cb_context:set_doc(Doc2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the username of the user who is authenticating this request.
%% @end
%%--------------------------------------------------------------------
-spec get_modifying_username(api_object()).
get_modifying_username(Doc) ->
    OwnerId = wh_json:get_value(<<"owner_id">>, Doc),
    AccountDb = wh_util:format_account_id(wh_json:get_value(<<"account_id">>), 'encoded'),
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, ?USERNAME_LIST, ViewOptions) of
        {'ok', [Username]} ->
            Username
    end.