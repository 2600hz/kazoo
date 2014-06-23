%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(crossbar_view).

-include("crossbar.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([load_view/3, load_view/4]).


-spec load_view(ne_binary(), wh_proplist(), cb_context:context()) ->
                           cb_context:context().
-spec load_view(ne_binary(), wh_proplist(), cb_context:context(), filter_fun() | 'undefined') -> 
                           cb_context:context().
load_view(View, ViewOptions, Context) ->
    fetch_results(View, ViewOptions, cb_context:set_doc(Context, []), 'undefined').
load_view(View, ViewOptions, Context, FilterFun) ->
    fetch_results(View, ViewOptions, cb_context:set_doc(Context, []), FilterFun).

%% ====================================================================
%% Internal functions
%% ====================================================================

-type filter_fun() :: fun((wh_json:object(), wh_json:objects()) -> wh_json:objects()).

%% -spec remove_qs_keys(cb_context:context()) -> cb_context:context().
%% remove_qs_keys(Context) ->
%%     cb_context:set_query_string(Context, wh_json:delete_keys([<<"created_from">>
%%                                                               ,<<"created_to">>
%%                                                              ]
%%                                                              ,cb_context:query_string(Context)
%%                                                             )).


-spec fetch_results(ne_binary(), wh_proplist(), cb_context:context(), filter_fun() | 'undefined') ->
                               cb_context:context().
fetch_results(View, ViewOptions, Context, FilterFun) ->
    case props:get_value('databases', ViewOptions, []) of
        [] ->
            fetch_results(View, ViewOptions, Context, [cb_context:account_db(Context)], FilterFun);
        DBList ->
            fetch_results(View, ViewOptions, Context, DBList, FilterFun)
    end.

-spec fetch_results(ne_binary(), wh_proplist(), cb_context:context(), ne_binaries(), filter_fun() | 'undefined') ->
                        cb_context:context().
fetch_results(View, ViewOptions, Context, Dbs, FilterFun) ->
    fetch_results(View, ViewOptions, Context, Dbs, cb_context:api_version(Context), FilterFun).

fetch_results(View, ViewOptions, Context, Dbs, <<"v1">>, FilterFun) ->
    fetch_results_v1(View, ViewOptions, Context, Dbs, FilterFun);
fetch_results(View, ViewOptions, Context, Dbs, _Version, FilterFun) ->
    fetch_paginated_results(View, ViewOptions, Context, Dbs, FilterFun).

-spec fetch_results_v1(ne_binary(), wh_proplist(), cb_context:context(), ne_binaries(), filter_fun() | 'undefined') ->
                           cb_context:context().
fetch_results_v1(_View, _ViewOptions, Context, [], _FilterFun) ->
    lager:debug("dbs exhausted"),
    Context;
fetch_results_v1(View, ViewOptions, Context, [Db|Dbs], FilterFun) ->
    C = crossbar_doc:load_view(View
                               ,ViewOptions
                               ,cb_context:set_account_db(Context, Db)
                               ,FilterFun
                              ),
    case cb_context:resp_status(C) of
        'success' ->
            JObjs = cb_context:doc(Context)
                ++ cb_context:doc(C),
            fetch_results_v1(View
                       ,ViewOptions
                       ,cb_context:set_resp_data(C, JObjs)
                       ,Dbs
                       ,FilterFun
                      );
        Else -> Else
    end.

-spec fetch_paginated_results(ne_binary(), wh_proplist(), cb_context:context(), ne_binaries(), filter_fun() | 'undefined') ->
                                  cb_context:context().
fetch_paginated_results(View, ViewOptions, Context, Dbs, FilterFun) ->
    ReqPageSize = crossbar_doc:pagination_page_size(Context),
    CurrentPageSize = wh_json:get_integer_value(<<"page_size">>, cb_context:resp_envelope(Context), 0),
    fetch_paginated_results(View, ViewOptions, Context, Dbs, FilterFun, ReqPageSize - CurrentPageSize).

-spec fetch_paginated_results(ne_binary(), wh_proplist(), cb_context:context(), ne_binaries(), filter_fun() | 'undefined', integer()) ->
                        cb_context:context().
fetch_paginated_results(_View, _ViewOptions, Context, [], _FilterFun, _PageSize) ->
    lager:debug("dbs exhausted"),
    Context;
fetch_paginated_results(_View, _ViewOptions, Context, _Dbs, _FilterFun, PageSize) when PageSize =< 0 ->
    lager:debug("page size exhausted"),
    Context;
fetch_paginated_results(View, ViewOptions, Context, [Db|Dbs], FilterFun, PageSize) ->
    C = crossbar_doc:load_view(View
                               ,ViewOptions
                               ,cb_context:set_account_db(Context, Db)
                               ,crossbar_doc:start_key(ViewOptions, Context)
                               ,PageSize
                               ,FilterFun
                              ),
    case cb_context:resp_status(C) of
        'success' ->
            JObjs = cb_context:doc(Context)
                ++ cb_context:doc(C),
            fetch_results(View
                       ,ViewOptions
                       ,cb_context:set_resp_envelope(
                          cb_context:set_resp_data(C, JObjs)
                          ,merge_resp_envelope(Context, C)
                         )
                       ,Dbs
                       ,FilterFun
                      );
        Else -> Else
    end.

-spec merge_resp_envelope(cb_context:context(), cb_context:context()) -> wh_json:object().
merge_resp_envelope(PriorContext, NewContext) ->
    maybe_fix_start_keys(
      wh_json:foldl(fun merge_resp_envelopes/3
                    ,cb_context:resp_envelope(NewContext)
                    ,cb_context:resp_envelope(PriorContext)
                   )).

-spec maybe_fix_start_keys(wh_json:object()) -> wh_json:object().
maybe_fix_start_keys(JObj) ->
    lists:foldl(fun maybe_fix_start_keys_fold/2
                ,JObj
                ,[<<"start_key">>, <<"next_start_key">>]
               ).

-spec maybe_fix_start_keys_fold(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_fix_start_keys_fold(Key, J) ->
    case wh_json:get_value(Key, J) of
        [_, Value] -> wh_json:set_value(Key, Value, J);
        _Value -> J
    end.

-spec merge_resp_envelopes(ne_binary(), term(), wh_json:object()) -> wh_json:object().
merge_resp_envelopes(<<"start_key">> = Key, [_, PriorValue], NewEnvelope) ->
    lager:debug("setting ~s to ~p, ignoring owner", [Key, PriorValue]),
    wh_json:set_value(Key, PriorValue, NewEnvelope);
merge_resp_envelopes(<<"start_key">> = Key, PriorValue, NewEnvelope) ->
    lager:debug("setting ~s to ~p", [Key, PriorValue]),
    wh_json:set_value(Key, PriorValue, NewEnvelope);
merge_resp_envelopes(<<"page_size">> = Key, PriorValue, NewEnvelope) ->
    NewValue = wh_json:get_integer_value(Key, NewEnvelope, 0),
    Sum = NewValue + PriorValue,
    lager:debug("updating ~s to ~p (~p + ~p)", [Key, Sum, NewValue, PriorValue]),
    wh_json:set_value(Key, Sum, NewEnvelope);
merge_resp_envelopes(_Key, _Value, NewEnvelope) ->
    lager:debug("skipping ~s: ~p", [_Key, _Value]),
    NewEnvelope.

%% -spec view_key_created_to(wh_proplist()) -> pos_integer().
%% view_key_created_to(Props) ->
%%     case props:get_value('startkey', Props) of
%%         [_, CreatedTo] -> CreatedTo;
%%         CreatedTo -> CreatedTo
%%     end.
%% 
%% -spec view_key_created_from(wh_proplist()) -> pos_integer().
%% view_key_created_from(Props) ->
%%     case props:get_value('endkey', Props) of
%%         [_, CreatedFrom] -> CreatedFrom;
%%         CreatedFrom -> CreatedFrom
%%     end.    
