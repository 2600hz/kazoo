%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CDR
%%% Read only access to CDR docs
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,validate/1, validate/2
        ]).

-include("include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).

-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_LIST, <<"cdrs/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.cdrs">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.cdrs">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.cdrs">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.cdrs">>, ?MODULE, validate).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/cdr/' can only accept GET
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/1 :: (#cb_context{}) -> #cb_context{}.
content_types_provided(#cb_context{}=Context) ->
    Context#cb_context{content_types_provided=[
                                               {to_json, [{<<"application">>, <<"json">>}]}
                                               ,{to_csv, [{<<"application">>, <<"octet-stream">>}]}
                                              ]
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
        load_cdr_summary(Context).
validate(#cb_context{req_verb = <<"get">>}=Context, CDRId) ->
        load_cdr(CDRId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_cdr_summary(#cb_context{req_nouns=Nouns, query_json=QJObj}=Context) ->
    case Nouns of
        [_, {?WH_ACCOUNTS_DB, [_AID]} | _] ->
            lager:debug("loading cdrs for account ~s", [_AID]),
            case create_view_options(undefined, Context) of
                {error, C} -> C;
                {ok, ViewOptions} ->
                    crossbar_doc:load_view(?CB_LIST
                                           ,ViewOptions
                                           ,Context#cb_context{query_json=wh_json:delete_keys([<<"created_to">>
                                                                                               ,<<"created_from">>
                                                                                              ], QJObj)}
                                           ,fun normalize_view_results/2)
            end;
        [_, {<<"users">>, [UserId] } | _] ->
            lager:debug("loading cdrs for user ~s", [UserId]),
            case create_view_options(UserId, Context) of
                {error, C} -> C;
                {ok, ViewOptions} ->
                    crossbar_doc:load_view(?CB_LIST_BY_USER
                                           ,ViewOptions
                                           ,Context#cb_context{query_json=wh_json:delete_keys([<<"created_to">>
                                                                                              ,<<"created_from">>
                                                                                              ], QJObj)}
                                           ,fun normalize_view_results/2)
            end;
        _ ->
            lager:debug("invalid URL chain for cdr summary request"),
            crossbar_util:response_faulty_request(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_cdr(CdrId, Context) ->
    crossbar_doc:load(CdrId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (Doc :: wh_json:json_object(), Acc :: wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [filter_cdr_fields(wh_json:get_value(<<"value">>, JObj))|Acc].

filter_cdr_fields(JObj) ->
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_view_options/2 :: ('undefined' | ne_binary(), #cb_context{}) -> {'error', #cb_context{}} |
                                                                             {'ok', proplist()}.
create_view_options(OwnerId, #cb_context{query_json=JObj}=Context) ->
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, 6048000),
    To = wh_json:get_integer_value(<<"created_to">>, JObj, wh_util:current_tstamp()),
    From = wh_json:get_integer_value(<<"created_from">>, JObj, wh_util:current_tstamp() - MaxRange),
    Diff = To - From,
    case {Diff < 0, Diff > MaxRange} of
        {true, _} -> 
            Error = wh_json:set_value([<<"created_from">>, <<"invalid">>]
                                      ,<<"created_from is prior to created_to">>
                                      ,wh_json:new()
                                     ),
            {error, crossbar_util:response_invalid_data(Error, Context)};
        {_, true} -> 
            Error = wh_json:set_value([<<"created_to">>, <<"invalid">>]
                                      ,<<"created_to is more than "
                                         ,(wh_util:to_binary(MaxRange))/binary
                                         ," seconds from created_from">>
                                         ,wh_json:new()),
            {error, crossbar_util:response_invalid_data(Error, Context)};
        {false, false} when OwnerId =:= undefined ->
            {ok, [{<<"startkey">>, From}, {<<"endkey">>, To}]};
        {false, false} ->
            {ok, [{<<"startkey">>, [OwnerId, From]}, {<<"endkey">>, [OwnerId, To]}]}
    end.
