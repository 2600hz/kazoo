-ifndef(KAPI_PRESENCE_API).


-define(DIALOG_STATES, [<<"early">>, <<"confirmed">>, <<"terminated">>]).

-define(PRESENCE_STATES, [<<"trying">>, <<"online">>, <<"offline">>
                              | ?DIALOG_STATES
                         ]).

%% Search request for active subscriptions
-define(SEARCH_REQ_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, [<<"Username">>, <<"Event-Package">>
                                     ,<<"Search-Type">>
                                     ]).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                           ,{<<"Event-Name">>, <<"search_req">>}
                           ]).
-define(SEARCH_REQ_TYPES, []).

%% Search partial response for active subscriptions
-define(SEARCH_PARTIAL_RESP_HEADERS, [<<"Subscriptions">>]).
-define(OPTIONAL_SEARCH_PARTIAL_RESP_HEADERS, []).
-define(SEARCH_PARTIAL_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                                    ,{<<"Event-Name">>, <<"search_partial_resp">>}
                                    ]).
-define(SEARCH_PARTIAL_RESP_TYPES, []).

%% Search response for active subscriptions
-define(SEARCH_RESP_HEADERS, []).
-define(OPTIONAL_SEARCH_RESP_HEADERS, [<<"Subscriptions">>]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"presence">>}
                            ,{<<"Event-Name">>, <<"search_resp">>}
                            ]).
-define(SEARCH_RESP_TYPES, []).

%% Presence subscription from Kamailio
-define(SUBSCRIBE_HEADERS, [<<"User">>, <<"Expires">>]).
-define(OPTIONAL_SUBSCRIBE_HEADERS, [<<"Queue">>, <<"From">>
                                    ,<<"Event-Package">>, <<"Call-ID">>
                                    ,<<"From-Tag">>, <<"To-Tag">>
                                    ,<<"Contact">>
                                    ]).
-define(SUBSCRIBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                          ,{<<"Event-Name">>, <<"subscription">>}
                          ]).
-define(SUBSCRIBE_TYPES, [{<<"Expires">>, fun(V) -> is_integer(kz_term:to_integer(V)) end}]).

%% Presence state updates
-define(UPDATE_HEADERS, [<<"Presence-ID">>, <<"State">>]).
-define(OPTIONAL_UPDATE_HEADERS, [<<"To">>, <<"To-Tag">>
                                 ,<<"From">>, <<"From-Tag">>
                                 ,<<"Call-Direction">>, <<"Call-ID">>
                                 ,<<"Target-Call-ID">>, <<"Switch-URI">>
                                 ,<<"Presence-ID">>, <<"Event-Package">>
                                 ,<<"From-User">>, <<"From-Realm">>
                                 ,<<"To-User">>, <<"To-Realm">>
                                 ]).
-define(UPDATE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"update">>}
                       ,{<<"State">>, ?PRESENCE_STATES}
                       ]).
-define(UPDATE_TYPES, []).

%% Presence_Probe
-define(PROBE_HEADERS, [<<"Username">>, <<"Realm">>, <<"Event-Package">>]).
-define(OPTIONAL_PROBE_HEADERS, [<<"From-User">>, <<"From-Realm">>
                                ,<<"To-User">>, <<"To-Realm">>
                                ,<<"Expires">>, <<"Call-ID">>
                                ]).
-define(PROBE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"probe">>}
                      ]).
-define(PROBE_TYPES, []).

%% MWI Update
-define(MWI_REQ_HEADERS, [<<"To">>
                         ,<<"Messages-New">>
                         ,<<"Messages-Saved">>
                         ]).
-define(OPTIONAL_MWI_REQ_HEADERS, [<<"Messages-Urgent">>
                                  ,<<"Messages-Urgent-Saved">>
                                  ,<<"Message-Account">>
                                  ,<<"Messages-Waiting">>
                                  ,<<"Call-ID">>, <<"Expires">>
                                  ,<<"Presence-ID">>
                                  ,<<"Extended-Presence-ID">>
                                  ,<<"From">>, <<"From-User">>, <<"From-Realm">>
                                  ,<<"To-User">>, <<"To-Realm">>
                                  ]).
-define(MWI_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                        ,{<<"Event-Name">>, <<"mwi_update">>}
                        ]).
-define(MWI_UNSOLICITED_REQ_VALUES, [{<<"Event-Category">>, <<"presence">>}
                                    ,{<<"Event-Name">>, <<"mwi_unsolicited_update">>}
                                    ]).
-define(MWI_REQ_TYPES, [{<<"Messages-New">>, fun is_integer/1}
                       ,{<<"Messages-Saved">>, fun is_integer/1}
                       ,{<<"Messages-Urgent">>, fun is_integer/1}
                       ,{<<"Messages-Urgent-Saved">>, fun is_integer/1}
                       ]).

%% MWI Query
-define(MWI_QUERY_HEADERS, [<<"Username">>, <<"Realm">>]).
-define(OPTIONAL_MWI_QUERY_HEADERS, [<<"Call-ID">>]).
-define(MWI_QUERY_VALUES, [{<<"Event-Category">>, <<"presence">>}
                          ,{<<"Event-Name">>, <<"mwi_query">>}
                          ]).
-define(MWI_QUERY_TYPES, []).

%% Register_Overwrite
-define(REGISTER_OVERWRITE_HEADERS, [<<"Previous-Contact">>, <<"Contact">>
                                    ,<<"Username">>, <<"Realm">>
                                    ]).
-define(OPTIONAL_REGISTER_OVERWRITE_HEADERS, []).
-define(REGISTER_OVERWRITE_VALUES, [{<<"Event-Category">>, <<"presence">>}
                                   ,{<<"Event-Name">>, <<"register_overwrite">>}
                                   ]).
-define(REGISTER_OVERWRITE_TYPES, []).

%% Flush presence dialog cache
-define(FLUSH_HEADERS, [<<"Type">>]).
-define(OPTIONAL_FLUSH_HEADERS, [<<"User">>, <<"Event-Package">>]).
-define(FLUSH_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"flush">>}
                      ]).
-define(FLUSH_TYPES, []).

%% Reset presence dialog cache entry
-define(RESET_HEADERS, [<<"Realm">>, <<"Username">>]).
-define(OPTIONAL_RESET_HEADERS, [<<"Event-Package">>]).
-define(RESET_VALUES, [{<<"Event-Category">>, <<"presence">>}
                      ,{<<"Event-Name">>, <<"reset">>}
                      ]).
-define(RESET_TYPES, []).

%% Sync presence
-define(SYNC_HEADERS, [<<"Action">>]).
-define(OPTIONAL_SYNC_HEADERS, [<<"Event-Package">>]).
-define(SYNC_VALUES, [{<<"Event-Category">>, <<"presence">>}
                     ,{<<"Event-Name">>, <<"sync">>}
                     ,{<<"Action">>, [<<"Request">>, <<"Start">>, <<"End">>]}
                     ]).
-define(SYNC_TYPES, []).

%% Dialog state updates
-define(DIALOG_HEADERS, [<<"To">>, <<"From">>]).
-define(OPTIONAL_DIALOG_HEADERS, [<<"Call-ID">>, <<"Direction">>
                                 ,<<"Event-Package">>, <<"State">>
                                 ,<<"From-Tag">>, <<"To-Tag">>
                                 ,<<"From-User">>, <<"From-Realm">>, <<"From-URI">>
                                 ,<<"To-User">>, <<"To-Realm">>, <<"To-URI">>
                                 ,<<"Expires">>, <<"Flush-Level">>
                                 ,<<"Presentity">>, <<"Presentity-User">>, <<"Presentity-Realm">>
                                 ,<<"Target-Call-ID">>, <<"Switch-URI">>, <<"Call-Cookie">>
                                 ,<<"Presence-ID">>
                                 ]).
-define(DIALOG_VALUES, [{<<"Event-Category">>, <<"presence">>}
                       ,{<<"Event-Name">>, <<"dialog_update">>}
                       ,{<<"State">>, ?DIALOG_STATES}
                       ]).
-define(DIALOG_TYPES, []).

-define(KAPI_PRESENCE_API, 'true').
-endif.
