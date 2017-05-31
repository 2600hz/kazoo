%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_port_requests).
%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([cleanup/1
        ,unconfirmed_port_reminder/1
        ]).

-include("tasks.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).

-define(UNFINISHED_PORT_REQUEST_LIFETIME,
        kapps_config:get_integer(?CONFIG_CAT, <<"unfinished_port_request_lifetime_s">>, ?SECONDS_IN_DAY * 30)).


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_SYSTEM, ?MODULE, cleanup),
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'unconfirmed_port_reminder').

%%% Triggerables

%% @public
%% @doc
%% Cleanup expired port requests
%% @end
-spec cleanup(ne_binary()) -> 'ok'.
cleanup(?KZ_PORT_REQUESTS_DB = Db) ->
    ModifiedBefore = kz_time:current_tstamp() - ?UNFINISHED_PORT_REQUEST_LIFETIME,
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

-spec unconfirmed_port_reminder(ne_binary()) -> 'ok'.
unconfirmed_port_reminder(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    ViewOpts = [{'startkey', [AccountId, ?PORT_UNCONFIRMED, kz_json:new()]}
               ,{'endkey', [AccountId, ?PORT_UNCONFIRMED]}
               ,'descending'
               ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?LISTING_BY_STATE, ViewOpts) of
        {'ok', []} -> lager:debug("no unfinished port requests");
        {'ok', Unfinished} -> unconfirmed_port_reminder(AccountId, Unfinished);
        {'error', _E} -> lager:debug("failed to query old port requests: ~p", [_E])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec cleanup(ne_binary(), kz_json:objects()) -> 'ok'.
cleanup(Db, OldPortRequests) ->
    lager:debug("checking ~b old port requests", [length(OldPortRequests)]),
    Deletable = [kz_json:get_value(<<"doc">>, OldPortRequest)
                 || OldPortRequest <- OldPortRequests,
                    should_delete_port_request(kz_json:get_value(<<"key">>, OldPortRequest))
                ],
    lager:debug("found ~p deletable", [length(Deletable)]),
    kz_datamgr:del_docs(Db, Deletable),
    'ok'.

-spec should_delete_port_request([pos_integer() | ne_binary(),...]) -> boolean().
should_delete_port_request([_Modified, ?PORT_SUBMITTED]) -> false;
should_delete_port_request([_Modified, ?PORT_SCHEDULED]) -> false;
should_delete_port_request(_) -> true.

-spec unconfirmed_port_reminder(ne_binary(), kz_json:objects()) -> 'ok'.
unconfirmed_port_reminder(AccountId, UnfinishedPorts) ->
    lager:debug("found ~p unfinished port requests, sending notifications", [length(UnfinishedPorts)]),
    F = fun (Port) -> send_port_unconfirmed_notification(AccountId, kz_doc:id(Port)) end,
    lists:foreach(F, UnfinishedPorts).

-spec send_port_unconfirmed_notification(ne_binary(), ne_binary()) -> 'ok'.
send_port_unconfirmed_notification(?NE_BINARY = AccountId, Id) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_unconfirmed/1).

%%% End of Module.
