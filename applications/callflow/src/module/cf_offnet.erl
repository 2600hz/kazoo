%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "to_did":"+14155550987" // statically dial DID
%%%   ,"media":"media_id"
%%%   ,"ringback":"ringback_id"
%%%   ,"format_from_did":boolean()
%%%   ,"timeout":integer()
%%%   ,"do_not_normalize":boolean()
%%%   ,"bypass_e164":boolean()
%%%   ,"from_uri_realm":"realm.com"
%%%   ,"caller_id_type":"external" // can use custom caller id properties on endpoints
%%%   ,"use_local_resources":boolean()
%%%   ,"hunt_account_id":"account_3" // use account_3's local carriers instead of current account
%%%   ,"emit_account_id":boolean() // puts account id in SIP header X-Account-ID
%%%   ,"custom_sip_headers:{"header":"value",...}
%%%   ,"ignore_early_media":boolean()
%%%   ,"outbound_flags":["flag_1","flag_2"] // used to match flags on carrier docs
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_offnet).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    cf_resources:handle(kz_json:set_value(<<"use_local_resources">>, 'false', Data), Call).
