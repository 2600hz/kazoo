%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_port_requests).

%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([cleanup/1
        ,unconfirmed_port_reminder/1
        ]).

-include("tasks.hrl").
-include_lib("kazoo_numbers/include/knm_port_request.hrl").

-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).

-define(UNFINISHED_PORT_REQUEST_LIFETIME
       ,kapps_config:get_integer(?CONFIG_CAT, <<"unfinished_port_request_lifetime_s">>, ?SECONDS_IN_DAY * 30)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_SYSTEM, ?MODULE, 'cleanup'),
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'unconfirmed_port_reminder').

%%% Triggerables

%% @doc Cleanup expired port requests
%% @end
-spec cleanup(kz_term:ne_binary()) -> 'ok'.
cleanup(?KZ_PORT_REQUESTS_DB = Db) ->
    ModifiedBefore = kz_time:now_s() - ?UNFINISHED_PORT_REQUEST_LIFETIME,
    ViewOpts = [{'startkey', [0]}
               ,{'endkey', [ModifiedBefore]}
               ,{'limit', kz_datamgr:max_bulk_insert()}
               ,'include_docs'
               ],
    case kz_datamgr:get_results(Db, <<"port_requests/listing_by_modified">>, ViewOpts) of
        {'ok', []} -> lager:debug("no port requests older than ~p", [ModifiedBefore]);
        {'ok', OldPortReqeusts} -> cleanup(Db, OldPortReqeusts);
        {'error', _E} -> lager:debug("failed to query old port requests: ~p", [_E])
    end;
cleanup(_) -> 'ok'.

-spec unconfirmed_port_reminder(kz_term:ne_binary()) -> 'ok'.
unconfirmed_port_reminder(AccountDb) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    ViewOpts = [{'startkey', [AccountId, ?PORT_UNCONFIRMED, kz_json:new()]}
               ,{'endkey', [AccountId, ?PORT_UNCONFIRMED]}
               ,'descending'
               ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?LISTING_BY_STATE, ViewOpts) of
        {'ok', []} -> lager:debug("no unfinished port requests");
        {'ok', Unfinished} -> unconfirmed_port_reminder(AccountId, Unfinished);
        {'error', _E} -> lager:debug("failed to query old port requests: ~p", [_E])
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
cleanup(Db, OldPortRequests) ->
    lager:debug("checking ~b old port requests", [length(OldPortRequests)]),
    Deletable = [kz_json:get_json_value(<<"doc">>, OldPortRequest)
                 || OldPortRequest <- OldPortRequests,
                    should_delete_port_request(kz_json:get_value(<<"key">>, OldPortRequest))
                ],
    lager:debug("found ~p deletable", [length(Deletable)]),
    _ = kz_datamgr:del_docs(Db, Deletable),
    'ok'.

-spec should_delete_port_request([pos_integer() | kz_term:ne_binary(),...]) -> boolean().
should_delete_port_request([_Modified, ?PORT_SUBMITTED]) -> 'false';
should_delete_port_request([_Modified, ?PORT_SCHEDULED]) -> 'false';
should_delete_port_request(_) -> 'true'.

-spec unconfirmed_port_reminder(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
unconfirmed_port_reminder(AccountId, UnfinishedPorts) ->
    lager:debug("found ~p unfinished port requests, sending notifications", [length(UnfinishedPorts)]),
    F = fun (Port) -> send_port_unconfirmed_notification(AccountId, kz_doc:id(Port)) end,
    lists:foreach(F, UnfinishedPorts).

-spec send_port_unconfirmed_notification(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_port_unconfirmed_notification(?NE_BINARY = AccountId, Id) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_unconfirmed/1).

%%% End of Module.
