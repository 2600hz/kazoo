%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_search).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
        ]).

-include("../crossbar.hrl").

-define(QUERY_TPL, <<"search/search_by_">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.search">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.search">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.search">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Type = cb_context:req_value(Context, <<"t">>),
    Ctx = case cb_context:account_id(Context) of
              'undefined' -> cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB);
              _AccountId -> Context
          end,
    validate_search(Ctx, Type).

-spec validate_search(cb_context:context(), api_binary()) -> cb_context:context().
validate_search(Context, 'undefined') ->
    cb_context:add_validation_error(
        <<"t">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Search needs to know what to look for">>}
         ])
        ,Context
    );
validate_search(Context, Type) ->
    validate_search(Context, cb_context:req_value(Context, <<"q">>), Type).

-spec validate_search(cb_context:context(), api_binary(), ne_binary()) -> cb_context:context().
validate_search(Context, 'undefined', _) ->
    cb_context:add_validation_error(
        <<"q">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Search needs to know what to look for">>}
         ])
        ,Context
    );
validate_search(Context, Q, T) ->
    validate_search(Context, cb_context:req_value(Context, <<"v">>), Q, T).

-spec validate_search(cb_context:context(), api_binary(), ne_binary(), ne_binary()) -> cb_context:context().
validate_search(Context, 'undefined', _, _) ->
    cb_context:add_validation_error(
        <<"v">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Search needs to know what to look for">>}
         ])
        ,Context
    );
validate_search(Context, V, Q, T) ->
    search(Context, V, Q, T).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec search(cb_context:context(), api_binary(), ne_binary(), ne_binary()) -> cb_context:context().
search(Context, Start, Field, Type) ->
    ViewName = <<?QUERY_TPL/binary, Field/binary>>,
    ViewOptions =
        [{'startkey', get_start_key(Context, Type, Start)}
         ,{'endkey', get_end_key(Context, Type, Start)}
         ,{'limit', crossbar_doc:pagination_page_size(Context)}
        ],
    fix_envelope(
      crossbar_doc:load_view(
        ViewName
        ,ViewOptions
        ,Context
        ,fun normalize_view_results/2
       )
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_start_key(cb_context:context(), ne_binary(), ne_binary()) -> ne_binaries().
get_start_key(Context, Type, Start) ->
    StartKey =
        case cb_context:req_value(Context, <<"start_key">>) of
            'undefined' -> Start;
            Key -> Key
        end,
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, StartKey];
        _ ->
            [Type, StartKey]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_end_key(cb_context:context(), ne_binary(), ne_binary()) -> ne_binaries().
get_end_key(Context, Type, Start) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, next_binary_key(Start)];
        _ ->
            [Type, next_binary_key(Start)]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% replaces last character in binary with next character
%% @end
%%--------------------------------------------------------------------
-spec next_binary_key(binary()) -> binary().
next_binary_key(Bin) ->
    <<Bin/binary, "\ufff0">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    cb_context:set_resp_envelope(
      cb_context:set_resp_data(Context, lists:reverse(cb_context:resp_data(Context)))
      ,lists:foldl(
         fun fix_envelope_fold/2
         ,cb_context:resp_envelope(Context)
         ,[<<"start_key">>, <<"next_start_key">>]
        )
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_envelope_fold(binary(), wh_json:object()) -> wh_json:object().
fix_envelope_fold(Key, JObj) ->
    case fix_start_key(wh_json:get_value(Key, JObj)) of
        'undefined' -> wh_json:delete_key(Key, JObj);
        V -> wh_json:set_value(Key, V, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_start_key(api_binaries()) -> api_binary().
fix_start_key('undefined') -> 'undefined';
fix_start_key([_, StartKey]) -> StartKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
