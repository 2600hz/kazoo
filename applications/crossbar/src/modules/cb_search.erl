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
         ,authorize/1, authorize/2
         ,resource_exists/0
         ,validate/1
        ]).

-include("../crossbar.hrl").

-define(QUERY_TPL, <<"search/search_by_">>).

-define(ACCOUNT_QUERY_OPTIONS, [<<"name">>, <<"number">>, <<"name_and_number">>]).
-define(ACCOUNTS_QUERY_OPTIONS, [<<"name">>]).

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
    _ = crossbar_bindings:bind(<<"*.validate.search">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorize the request
%% @end

-spec authorize(cb_context:context()) -> 'false'.
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(Context) ->
    cb_context:auth_token(Context) =/= 'undefined'.

authorize(Context, _Module) ->
    cb_context:auth_token(Context) =/= 'undefined'.

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
      ,wh_json:from_list([{<<"message">>, <<"Search needs a document type to search on">>}])
      ,Context
     );
validate_search(Context, Type) ->
    validate_search(Context, Type, cb_context:req_value(Context, <<"q">>)).

-spec validate_search(cb_context:context(), ne_binary(), api_binary()) ->
                             cb_context:context().
validate_search(Context, _Type, 'undefined') ->
    cb_context:add_validation_error(
      <<"q">>
      ,<<"required">>
      ,wh_json:from_list([{<<"message">>, <<"Search needs a view to search in">>}])
      ,Context
     );
validate_search(Context, Type, Query) ->
    Context1 = validate_query(Context, Query),
    case cb_context:resp_status(Context1) of
        'success' ->
            validate_search(Context, Type, Query, cb_context:req_value(Context, <<"v">>));
        _Status ->
            Context1
    end.

-spec validate_query(cb_context:context(), ne_binary()) -> cb_context:context().
-spec validate_query(cb_context:context(), ne_binary(), ne_binaries()) -> cb_context:context().
validate_query(Context, Query) ->
    case cb_context:account_db(Context) of
        ?WH_ACCOUNTS_DB ->
            validate_query(Context, Query, ?ACCOUNTS_QUERY_OPTIONS);
        _AccountId ->
            validate_query(Context, Query, ?ACCOUNT_QUERY_OPTIONS)
    end.

validate_query(Context, Query, Available) ->
    case lists:member(Query, Available) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_validation_error(
              <<"q">>
              ,<<"enum">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"Value not found in enumerated list of values">>}
                  ,{<<"target">>, Available}
                 ])
              ,Context
             )
    end.

-spec validate_search(cb_context:context(), ne_binary(), ne_binary(), binary() | 'undefined') ->
                             cb_context:context().
validate_search(Context, _Type, _Query, 'undefined') ->
    cb_context:add_validation_error(
      <<"v">>
      ,<<"required">>
      ,wh_json:from_list([{<<"message">>, <<"Search needs a value to search with">>}])
      ,Context
     );
validate_search(Context, Type, Query, <<_/binary>> = Value) ->
    search(Context, Type, Query, Value);
validate_search(Context, Type, Query, Value) ->
    case wh_util:is_true(Value) of
        'true' -> validate_search(Context, Type, Query, <<>>);
        'false' -> validate_search(Context, Type, Query, 'undefined')
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec search(cb_context:context(), ne_binary(), ne_binary(), binary()) -> cb_context:context().
search(Context, Type, Query, Value) ->
    ViewName = <<?QUERY_TPL/binary, Query/binary>>,
    ViewOptions =
        [{'startkey', get_start_key(Context, Type, Value)}
         ,{'endkey', get_end_key(Context, Type, Value)}
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
get_start_key(Context, Type, Value) ->
    StartKey =
        case cb_context:req_value(Context, <<"start_key">>) of
            'undefined' -> Value;
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
-spec get_end_key(cb_context:context(), ne_binary(), binary()) -> ne_binaries().
get_end_key(Context, Type, Value) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, next_binary_key(Value)];
        _ ->
            [Type, next_binary_key(Value)]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% replaces last character in binary with next character
%% @end
%%--------------------------------------------------------------------
-spec next_binary_key(binary()) -> ne_binary().
next_binary_key(<<>>) ->
    <<"\ufff0">>;
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
fix_start_key([_ , StartKey]) -> StartKey;
fix_start_key([_ , _, StartKey]) -> StartKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
