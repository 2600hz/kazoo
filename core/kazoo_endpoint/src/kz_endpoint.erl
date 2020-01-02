%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoint).

-export([get/1, get/2]).
-export([flush_account/1
        ,flush_account_local/1
        ,flush/2
        ,flush_local/2
        ]).
-export([build/2, build/3]).
-export([create_call_fwd_endpoint/3
        ,create_sip_endpoint/3
        ,maybe_start_metaflow/2
        ,encryption_method_map/2
        ,get_sip_realm/2, get_sip_realm/3
        ]).

-ifdef(TEST).
-export([attributes_keys/0
        ,merge_attribute/4
        ]).

-include_lib("eunit/include/eunit.hrl").
-endif.

-include("kazoo_endpoint.hrl").

-type std_return() :: {'ok', kz_json:object()} |
                      {'error', 'invalid_endpoint_id'} |
                      kz_datamgr:data_error().
-export_type([std_return/0]).

%%------------------------------------------------------------------------------
%% @doc Fetches a endpoint definition from the database or cache
%% @end
%%------------------------------------------------------------------------------
-spec get(kapps_call:call()) -> std_return().
get(Call) -> ?MOD:get(Call).

-spec get(kz_term:api_binary(), kz_term:ne_binary() | kapps_call:call()) -> std_return().
get(EndpointId, _Call) -> ?MOD:get(EndpointId, _Call).


-ifdef(TEST).

attributes_keys() -> ?MOD:attributes_keys().

-spec merge_attribute(kz_term:ne_binary(), kz_term:api_object(), kz_term:api_object(), kz_term:api_object()) -> kz_json:object().
merge_attribute(Key, Account, Endpoint, Owner) -> ?MOD:merge_attribute(Key, Account, Endpoint, Owner).

-endif.

%%------------------------------------------------------------------------------
%% @doc Flush the callflow cache
%% @end
%%------------------------------------------------------------------------------

-spec flush_account(kz_term:ne_binary()) -> 'ok'.
flush_account(AccountDb) -> ?MOD:flush_account(AccountDb).

-spec flush_account_local(kz_term:ne_binary()) -> 'ok'.
flush_account_local(AccountDb) -> ?MOD:flush_account_local(AccountDb).

-spec flush(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush(Db, Id) -> ?MOD:flush(Db, Id).

-spec flush_local(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush_local(Db, Id) -> ?MOD:flush_local(Db, Id).

%%------------------------------------------------------------------------------
%% @doc Creates one or more kazoo API endpoints for use in a bridge string.
%% Takes into account settings on the callflow, the endpoint, call
%% forwarding, and ringtones.  More functionality to come, but as it is
%% added it will be implicit in all functions that 'ring an endpoint'
%% like devices, ring groups, and resources.
%% @end
%%------------------------------------------------------------------------------
-type build_errors() :: 'db_not_reachable' | 'endpoint_disabled'
                      | 'endpoint_called_self' | 'endpoint_id_undefined'
                      | 'invalid_endpoint_id' | 'not_found' | 'owner_called_self'
                      | 'do_not_disturb' | 'no_resource_type'.


-spec build(kz_term:api_ne_binary() | kz_json:object(), kapps_call:call()) ->
          {'ok', kz_json:objects()} |
          {'error', build_errors()}.
build(EndpointId, Call) -> ?MOD:build(EndpointId, Call).

-spec build(kz_term:api_ne_binary() | kz_json:object(), kz_term:api_object(), kapps_call:call()) ->
          {'ok', kz_json:objects()} |
          {'error', build_errors()}.
build(EndpointId, Properties, Call) -> ?MOD:build(EndpointId, Properties, Call).

-spec maybe_start_metaflow(kapps_call:call(), kz_json:object()) -> 'ok'.
maybe_start_metaflow(Call, Endpoint) -> ?MOD:maybe_start_metaflow(Call, Endpoint).

%%------------------------------------------------------------------------------
%% @doc Creates the kazoo API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonly a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%------------------------------------------------------------------------------

-spec create_sip_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
          kz_json:object().
create_sip_endpoint(Endpoint, Properties, Call) -> ?MOD:create_sip_endpoint(Endpoint, Properties, Call).


%%------------------------------------------------------------------------------
%% @doc Creates the Kazoo API endpoint for a bridge call command when
%% the device (or owner) has forwarded their phone.  This endpoint
%% is comprised of a route based on CallFwd, the relevant settings
%% from the actually endpoint, and the properties of this endpoint in
%% the callflow.
%% @end
%%------------------------------------------------------------------------------
-spec create_call_fwd_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
          kz_json:object().
create_call_fwd_endpoint(Endpoint, Properties, Call) -> ?MOD:create_call_fwd_endpoint(Endpoint, Properties, Call).


-spec encryption_method_map(kz_term:api_object(), kz_term:api_binaries() | kz_json:object()) -> kz_term:api_object().
encryption_method_map(CCVs, Methods) -> ?MOD:encryption_method_map(CCVs, Methods).

%%------------------------------------------------------------------------------
%% @doc Get the sip realm
%% @end
%%------------------------------------------------------------------------------
-spec get_sip_realm(kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binary().
get_sip_realm(SIPJObj, AccountId) -> ?MOD:get_sip_realm(SIPJObj, AccountId).

-spec get_sip_realm(kz_json:object(), kz_term:ne_binary(), Default) -> Default | kz_term:ne_binary().
get_sip_realm(SIPJObj, AccountId, Default) -> ?MOD:get_sip_realm(SIPJObj, AccountId, Default).
