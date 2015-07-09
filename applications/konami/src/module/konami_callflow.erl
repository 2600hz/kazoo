%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Overlay a callflow onto the caller
%%% data:{
%%%   "id":"callflow_id"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_callflow).

-export([handle/2]).

-include("../konami.hrl").

-define(LIST_BY_NUMBER, <<"callflow/listing_by_number">>).

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'stop', whapps_call:call()}.
handle(Data, Call) ->
    {'ok', CallflowJObj} = callflow(Data, Call),
    Flow = wh_json:get_value(<<"flow">>, CallflowJObj),
    API = [{<<"Call">>, whapps_call:to_json(Call)}
           ,{<<"Flow">>, Flow}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_callflow:publish_resume(API),
    {'stop', Call}.

callflow(Data, Call) ->
    callflow(Data, Call, wh_doc:id(Data)).

callflow(Data, Call, 'undefined') ->
    captured_callflow(Call, wh_json:get_first_defined([<<"captures">>
                                                       ,<<"collected">>
                                                      ], Data));
callflow(_Data, Call, CallflowId) ->
    couch_mgr:open_cache_doc(whapps_call:account_db(Call)
                             ,CallflowId
                            ).

captured_callflow(_Call, 'undefined') -> 'undefined';
captured_callflow(Call, [Number]) ->
    captured_callflow(Call, Number);
captured_callflow(Call, Number) ->
    Options = [{'key', Number}, 'include_docs'],
    case couch_mgr:get_results(whapps_call:account_db(Call), ?LIST_BY_NUMBER, Options) of
        {'ok', [JObj]} -> {'ok', wh_json:get_value(<<"doc">>, JObj)};
        E -> E
    end.
