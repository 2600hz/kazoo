%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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

-define(QUERY_ACCOUNTS, <<"accounts/search_by_name">>).

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
    Index = cb_context:req_value(Context, <<"c">>),
    validate_search(Context, Index).

-spec validate_search(cb_context:context(), api_binary()) -> cb_context:context().
validate_search(Context, <<"accounts">>) ->
    Q = cb_context:req_value(Context, <<"q">>),
    validate_search_accounts(cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB), Q);
validate_search(Context, _) ->
    cb_context:add_validation_error(<<"ctx">>, <<"required">>, <<"query needs context">>, Context).


-spec validate_search_accounts(cb_context:context(), api_binary()) -> cb_context:context().
validate_search_accounts(Context, 'undefined') ->
    cb_context:add_validation_error(<<"ctx">>, <<"required">>, <<"query needs context">>, Context);
validate_search_accounts(Context, Q) ->
    Lines  = binary:split(wh_util:uri_decode(Q), <<"&">>, ['global']),
    Query = lists:map(fun(A) ->
                      [Field, Value] = binary:split(A, <<"=">>),
                      {Field, Value}
              end, Lines),
    search(Context, <<"accounts">>, Query).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec search(cb_context:context(), ne_binary(), wh_proplist()) -> cb_context:context().
search(Context, <<"accounts">>, Query) ->
    AccountId = cb_context:auth_account_id(Context),
    Name = props:get_value(<<"name">>, Query),
    End = get_end_key(Name),
    ViewOptions = [{'startkey', [AccountId, Name]}
                   ,{'endkey', [AccountId, End]}
                   ,{'limit', crossbar_doc:pagination_page_size(Context)}
                  ],
    crossbar_doc:load_view(?QUERY_ACCOUNTS, ViewOptions, Context, fun normalize_view_results/2);
search(Context, _Index, _Query) ->
    Context.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% replaces last character in binary with next character
%% @end
%%--------------------------------------------------------------------
-spec get_end_key(binary()) -> binary().
get_end_key(Bin) ->
    L = binary:bin_to_list(Bin),
    C = lists:last(L)+1,
    B = binary:list_to_bin(lists:sublist(L, 1, length(L) - 1)),
    <<B/binary, C>>.
