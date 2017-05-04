-ifndef(CB_CALLFLOWS_TEST_HRL).

-include_lib("eunit/include/eunit.hrl").

-define(USER_VM_FLOW_JSON, <<"{\"children\":{\"_\": {\"children\": {},\"data\": {\"id\": \"{VM_ID}\"},\"module\": \"voicemail\"}},"
                             "\"data\": {\"can_call_self\": false,\"id\": \"{USER_ID}\",\"timeout\": \"20\"},\"module\": \"user\"}"
                           >>).
-define(USER_VM_METADATA_JSON, <<"{\"{VM_ID}\": {\"name\": \"vm1\",\"pvt_type\": \"vmbox\"},"
                                 "\"{USER_ID}\": {\"name\": \"User Name\",\"pvt_type\": \"user\"}}"
                               >>).

-define(RING_GROUP_TOGGLE_JSON, <<"{\"children\":{},\"data\":{\"action\":\"login\",\"callflow_id\":\"{RING_GROUP_ID}\"},\"module\":\"ring_group_toggle\"}">>).
-define(RING_GROUP_METADATA_JSON, <<"{\"{RING_GROUP_ID}\":{\"name\":\"support ring group\", \"pvt_type\":\"callflow\",\"numbers\":[\"1000\"]}}">>).

-define(NO_METADATA_FLOW, <<"{\"children\":{},\"data\":{\"foo\":\"bar\"},\"module\":\"nope\"}">>).
-define(NO_METADATA, <<"{}">>).

-define(TEST_USER, kz_json:from_list([{<<"first_name">>, <<"User">>}
                                     ,{<<"last_name">>, <<"Name">>}
                                     ,{<<"pvt_type">>, <<"user">>}
                                     ,{<<"_id">>, <<"{USER_ID}">>}
                                     ])).

-define(TEST_VM, kz_json:from_list([{<<"name">>, <<"vm1">>}
                                   ,{<<"pvt_type">>, <<"vmbox">>}
                                   ,{<<"_id">>, <<"{VM_ID}">>}
                                   ])).

-define(TEST_RING_GROUP, kz_json:from_list([{<<"name">>, <<"support ring group">>}
                                           ,{<<"numbers">>, [<<"1000">>]}
                                           ,{<<"pvt_type">>, <<"callflow">>}
                                           ,{<<"_id">>, <<"{RING_GROUP_ID}">>}
                                           ])).

metadata_test_() ->
    Tests = [{?USER_VM_FLOW_JSON, ?USER_VM_METADATA_JSON}
            ,{?RING_GROUP_TOGGLE_JSON, ?RING_GROUP_METADATA_JSON}
            ,{?NO_METADATA_FLOW, ?NO_METADATA}
            ],
    [metadata_test_gen(FlowJSON, MetadataJSON) || {FlowJSON, MetadataJSON} <- Tests].

metadata_test_gen(FlowJSON, MetadataJSON) ->
    ?_assert(
       kz_json:are_equal(get_metadata(kz_json:decode(FlowJSON), <<"db">>)
                        ,kz_json:decode(MetadataJSON)
                        )
      ).

-define(CB_CALLFLOWS_TEST_HRL, 'true').
-endif.
