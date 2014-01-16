%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,validate/1, validate/2
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).

-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_LIST, <<"cdrs/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cdrs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cdrs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.cdrs">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.cdrs">>, ?MODULE, 'validate').

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
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(#cb_context{}=Context) ->
    CTPs = [{'to_json', [{<<"application">>, <<"json">>}]}
            ,{'to_csv', [{<<"application">>, <<"octet-stream">>}]}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_cdr_summary(Context).
validate(#cb_context{req_verb = ?HTTP_GET}=Context, CDRId) ->
    load_cdr(CDRId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context()) -> cb_context:context().
load_cdr_summary(#cb_context{req_nouns=[_, {?WH_ACCOUNTS_DB, [_]} | _]}=Context) ->
    lager:debug("loading cdrs for account ~s", [cb_context:account_id(Context)]),
    case create_view_options('undefined', Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST
                      ,ViewOptions
                      ,cb_context:set_query_string(Context, wh_json:new()));
        Else -> Else
    end;
load_cdr_summary(#cb_context{req_nouns=[_, {<<"users">>, [UserId] } | _]}=Context) ->
    lager:debug("loading cdrs for user ~s", [UserId]),
    case create_view_options(UserId, Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST_BY_USER
                      ,ViewOptions
                      ,cb_context:set_query_string(Context, wh_json:new())
                     );
        Else -> Else
    end;
load_cdr_summary(Context) ->
    lager:debug("invalid URL chain for cdr summary request"),
    cb_context:add_system_error('faulty_request', Context).

-spec load_view(ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
load_view(View, ViewOptions, Context) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"always_show_cost">>, 'false')
        orelse (whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"show_resellers_cost">>, 'true')
                andalso wh_services:is_reseller(cb_context:auth_account_id(Context)))
        orelse (whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"show_toplevel_clients_cost">>, 'true')
                andalso (wh_services:find_reseller_id(cb_context:account_id(Context))
                         =:= master_account_id()))
    of
        'true' -> fetch_cdrs(View, ['include_docs'|ViewOptions], cb_context:set_doc(Context, []));
        'false' -> fetch_cdrs(View, ViewOptions, cb_context:set_doc(Context, []))
    end.

-spec fetch_cdrs(ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
fetch_cdrs(View, ViewOptions, Context) ->
    case {cdr_db(view_key_created_from(ViewOptions), Context)
          ,cdr_db(view_key_created_to(ViewOptions), Context)}
    of
        {Db, Db} -> fetch_cdrs([Db], View, ViewOptions, Context);
        {PastDb, PresentDb} ->
            fetch_cdrs([PastDb, PresentDb], View, ViewOptions, Context)
    end.

-spec fetch_cdrs(ne_binaries(), ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
fetch_cdrs([], _, _, Context) -> Context;
fetch_cdrs([Db|Dbs], View, ViewOptions, Context) ->
    C = crossbar_doc:load_view(View
                               ,ViewOptions
                               ,cb_context:set_account_db(Context, Db)
                               ,fun(JObj, JObjs) -> normalize_view_results(JObj, JObjs) end
                              ),
    case cb_context:resp_status(C) of
        'success' ->
            JObjs = cb_context:doc(Context)
                ++ cb_context:doc(C),
            fetch_cdrs(Dbs
                       ,View
                       ,ViewOptions
                       ,cb_context:set_resp_data(C, JObjs));
        Else -> Else
    end.

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, JObjs) ->
    Value = wh_json:get_value(<<"value">>, JObj),
    case wh_json:get_value(<<"doc">>, JObj) of
        'undefined' -> [Value|JObjs];
        Doc ->
            [wh_json:set_value(<<"cost">>, wht_util:call_cost(Doc), Value)
             |JObjs
            ]
    end.

-spec view_key_created_to(wh_proplist()) -> pos_integer().
view_key_created_to(Props) ->
    case props:get_value('startkey', Props) of
        [_, CreatedTo] -> CreatedTo;
        CreatedTo -> CreatedTo
    end.

-spec view_key_created_from(wh_proplist()) -> pos_integer().
view_key_created_from(Props) ->
    case props:get_value('endkey', Props) of
        [_, CreatedFrom] -> CreatedFrom;
        CreatedFrom -> CreatedFrom
    end.

-spec create_view_options(api_binary(), cb_context:context()) ->
                                 {'ok', wh_proplist()} |
                                 cb_context:context().
create_view_options(OwnerId, Context) ->
    TStamp =  wh_util:current_tstamp(),
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, 6048000),
    CreatedFrom = wh_util:to_integer(cb_context:req_value(Context, <<"created_from">>, TStamp - MaxRange)),
    CreatedTo = wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, CreatedFrom + MaxRange)),
    Diff = CreatedTo - CreatedFrom,
    if
        Diff < 0 ->
            Message = <<"created_from is prior to created_to">>,
            cb_context:add_validation_error(<<"created_from">>
                                            ,<<"date_range">>
                                            ,Message
                                            ,Context);
        Diff > MaxRange ->
            Message = <<"created_to is more than "
                        ,(wh_util:to_binary(MaxRange))/binary
                        ," seconds from created_from">>,
            cb_context:add_validation_error(<<"created_from">>
                                            ,<<"date_range">>
                                            ,Message
                                            ,Context);
        OwnerId =:= 'undefined' ->
            {'ok', [{'startkey', CreatedFrom}
                    ,{'endkey', CreatedTo}
                   ]};
        'true' ->
            {'ok', [{'startkey', [OwnerId, CreatedFrom]}
                    ,{'endkey', [OwnerId, CreatedTo]}
                   ]}
    end.

-spec master_account_id() -> ne_binary().
master_account_id() ->
    {'ok', AccountId} = whapps_util:get_master_account_id(),
    AccountId.

-spec cdr_db(pos_integer(), cb_context:context()) -> ne_binary().
cdr_db(Timestamp, Context) ->
    Db = cdr_db_name(Timestamp, Context),
    case couch_mgr:db_exists(Db) of
        'true' ->
            maybe_add_design_doc(Db),
            Db;
        'false' -> cb_context:account_db(Context)
    end.

-spec maybe_add_design_doc(ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_design_doc(AccountMODb) ->
    case couch_mgr:lookup_doc_rev(AccountMODb, <<"_design/cdrs">>) of
        {'error', 'not_found'} ->
            couch_mgr:load_doc_from_file(AccountMODb
                                         ,'crossbar'
                                         ,<<"account/cdrs.json">>);
        {'ok', _ } -> 'ok'
    end.

-spec cdr_db_name(pos_integer(), cb_context:context()) -> ne_binary().
-spec cdr_db_name(wh_year(), wh_month(), cb_context:context()) -> ne_binary().

cdr_db_name(Timestamp, Context) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    #cb_context{req_nouns=ReqNouns} = Context,
    [AccountId] = props:get_value(<<"accounts">>, ReqNouns),
    wh_util:format_account_id(AccountId, Year, Month).

cdr_db_name(Year, Month, Context) ->
    #cb_context{req_nouns=ReqNouns} = Context,
    [AccountId] = props:get_value(<<"accounts">>, ReqNouns),
    wh_util:format_account_id(AccountId, Year, Month).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr(ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(<<Year:4/binary, Month:2/binary, "-", _/binary>> = CDRId, Context) ->
    AcctDb = cdr_db_name(wh_util:to_integer(Year), wh_util:to_integer(Month), Context),
    Context1 = cb_context:set_account_db(Context,AcctDb),
    crossbar_doc:load(CDRId, Context1);
load_cdr(CDRId, Context) ->
    lager:debug("error loading cdr by id ~p", [CDRId]),
    crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context).
