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
         ,validate/1, validate/2
        ]).

-include("../crossbar.hrl").

-define(USER_LIST, <<"users/crossbar_listing">>).

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
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    update_modified(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    update_modified(Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context) ->
    update_modified(Context).
validate(Context, _) -> ?MODULE:validate(Context).


-spec update_modified(cb_context:context()) -> cb_context:context().
update_modified(Context) ->
    Doc1 = wh_json:set_value(<<"modified_by">>, get_modifying_username(cb_context:auth_doc(Context)), cb_context:req_data(Context)),
    Doc2 = wh_json:set_value(<<"modified_time">>, get_modified_time(), Doc1),
    cb_context:set_req_data(Context, Doc2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the username of the user who is authenticating this request.
%% @end
%%--------------------------------------------------------------------
-spec get_modifying_username(api_object()) -> ne_binary().
get_modifying_username(Doc) ->
    OwnerId = wh_json:get_binary_value(<<"owner_id">>, Doc),
    AccountDb = wh_util:format_account_id(wh_json:get_binary_value(<<"account_id">>, Doc), 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', User} ->
            wh_json:get_binary_value(<<"username">>, User)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the UTC time when this document was modified (ISO 8601 format).
%% @end
%%--------------------------------------------------------------------
-spec get_modified_time() -> ne_binary().
get_modified_time() ->
    {{Y,M,D},{H,Min,S}} = calendar:universal_time(),
    wh_util:to_binary([Y,"-",M,"-",D,"T",H,":",Min,":",S]).
