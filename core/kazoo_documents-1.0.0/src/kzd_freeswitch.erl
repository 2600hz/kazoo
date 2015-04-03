%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% FreeSWITCH proplists
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_freeswitch).

-export([caller_id_name/1, caller_id_name/2
         ,caller_id_number/1, caller_id_number/2
         ,call_id/1
         ,other_leg_call_id/1
         ,call_direction/1
         ,resource_type/1, resource_type/2
         ,channel_authorized/1
         ,hunt_destination_number/1
         ,is_channel_recovering/1, is_channel_recovering/2
         ,is_consuming_global_resource/1, is_consuming_global_resource/2
         ,resource_id/1

         ,authorizing_id/1
         ,authorizing_type/1

         ,account_id/1, account_billing/1
         ,reseller_id/1, reseller_billing/1

         ,to_did/1
        ]).

-include("kz_documents.hrl").

-define(CHANNEL_VAR_PREFIX, "ecallmgr_").
-define(CCV(Key), <<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>).

-spec caller_id_name(wh_proplist()) -> api_binary().
-spec caller_id_name(wh_proplist(), Default) -> ne_binary() | Default.
caller_id_name(Props) ->
    caller_id_name(Props, 'undefined').
caller_id_name(Props, Default) ->
    props:get_first_defined([<<"variable_effective_caller_id_name">>
                             ,<<"Caller-Caller-ID-Name">>
                            ]
                            ,Props
                            ,Default
                           ).

-spec caller_id_number(wh_proplist()) -> api_binary().
-spec caller_id_number(wh_proplist(), Default) -> ne_binary() | Default.
caller_id_number(Props) ->
    caller_id_number(Props, 'undefined').
caller_id_number(Props, Default) ->
    props:get_first_defined([<<"variable_effective_caller_id_number">>
                             ,<<"Caller-Caller-ID-Number">>
                            ]
                            ,Props
                            ,Default
                           ).

-spec call_id(wh_proplist()) -> api_binary().
call_id(Props) ->
    props:get_first_defined([<<"Unique-ID">>, <<"Call-ID">>], Props).

-spec other_leg_call_id(wh_proplist()) -> api_binary().
other_leg_call_id(Props) ->
    props:get_value(<<"Other-Leg-Unique-ID">>, Props).

-spec call_direction(wh_proplist()) -> api_binary().
call_direction(Props) ->
    props:get_binary_value(<<"Call-Direction">>, Props).

-spec resource_type(wh_proplist()) -> api_binary().
-spec resource_type(wh_proplist(), Default) -> ne_binary() | Default.
resource_type(Props) ->
    resource_type(Props, 'undefined').

resource_type(Props, Default) ->
    props:get_value(<<"Resource-Type">>, Props, Default).

-spec channel_authorized(wh_proplist()) -> api_binary().
channel_authorized(Props) ->
    props:get_value(?CCV(<<"Channel-Authorized">>), Props).

-spec hunt_destination_number(wh_proplist()) -> api_binary().
hunt_destination_number(Props) ->
    props:get_value(<<"Hunt-Destination-Number">>, Props).

-spec is_channel_recovering(wh_proplist()) -> boolean().
-spec is_channel_recovering(wh_proplist(), boolean()) -> boolean().
is_channel_recovering(Props) ->
    is_channel_recovering(Props, 'false').

is_channel_recovering(Props, Default) ->
    props:is_true(<<"variable_recovered">>, Props, Default).

-spec is_consuming_global_resource(wh_proplist()) -> api_boolean().
-spec is_consuming_global_resource(wh_proplist(), api_boolean()) -> api_boolean().
is_consuming_global_resource(Props) ->
    is_consuming_global_resource(Props, 'undefined').

is_consuming_global_resource(Props, Default) ->
    props:is_true(?CCV(<<"Global-Resource">>), Props, Default).

-spec resource_id(wh_proplist()) -> api_binary().
resource_id(Props) ->
    props:get_value(?CCV(<<"Resource-ID">>), Props).

-spec authorizing_id(wh_proplist()) -> api_binary().
authorizing_id(Props) ->
    props:get_value(?CCV(<<"Authorizing-ID">>), Props).

-spec authorizing_type(wh_proplist()) -> api_binary().
authorizing_type(Props) ->
    props:get_value(?CCV(<<"Authorizing-Type">>), Props).

-spec account_id(wh_proplist()) -> api_binary().
account_id(Props) ->
    props:get_value(?CCV(<<"Account-ID">>), Props).

-spec account_billing(wh_proplist()) -> api_binary().
account_billing(Props) ->
    props:get_binary_value(?CCV(<<"Account-Billing">>), Props).

-spec reseller_id(wh_proplist()) -> api_binary().
reseller_id(Props) ->
    props:get_value(?CCV(<<"Reseller-ID">>), Props).

-spec reseller_billing(wh_proplist()) -> api_binary().
reseller_billing(Props) ->
    props:get_binary_value(?CCV(<<"Reseller-Billing">>), Props).

-spec to_did(wh_proplist()) -> api_binary().
to_did(Props) ->
    props:get_first_defined([?CCV(<<"Original-Number">>)
                             ,<<"Caller-Destination-Number">>
                            ]
                            ,Props
                           ).
