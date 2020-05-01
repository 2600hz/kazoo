%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Overlay a callflow onto the caller
%%% data:{
%%%   "id":"callflow_id"
%%% }
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_callflow).

-export([handle/2]).

-include("konami.hrl").

-define(LIST_BY_NUMBER, <<"callflows/listing_by_number">>).

-spec handle(kz_json:object(), kapps_call:call()) ->
          {'stop', kapps_call:call()}.
handle(Data, Call) ->
    {'ok', CallflowJObj} = callflow(Data, Call),
    Flow = kz_json:get_value(<<"flow">>, CallflowJObj),
    API = [{<<"Call">>, kapps_call:to_json(Call)}
          ,{<<"Flow">>, Flow}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapi_callflow:publish_resume(API),
    {'stop', Call}.

callflow(Data, Call) ->
    callflow(Data, Call, kz_doc:id(Data)).

callflow(Data, Call, 'undefined') ->
    captured_callflow(Call, kz_json:get_first_defined([<<"captures">>
                                                      ,<<"collected">>
                                                      ], Data));
callflow(_Data, Call, CallflowId) ->
    kz_datamgr:open_cache_doc(kapps_call:account_db(Call)
                             ,CallflowId
                             ).

captured_callflow(_Call, 'undefined') -> 'undefined';
captured_callflow(Call, [Number]) ->
    captured_callflow(Call, Number);
captured_callflow(Call, Number) ->
    Options = [{'key', Number}, 'include_docs'],
    case kz_datamgr:get_results(kapps_call:account_db(Call), ?LIST_BY_NUMBER, Options) of
        {'ok', [JObj]} -> {'ok', kz_json:get_value(<<"doc">>, JObj)};
        E -> E
    end.
