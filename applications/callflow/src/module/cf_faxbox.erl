%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Instructs the switch to receive a fax from the caller.
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%%
%%% @author James Aimonetti
%%% @author Ben Wann
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_faxbox).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(FAX_OPTION, [<<"media">>, <<"fax_option">>]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    FaxboxId = get_faxbox_id(Data),
    lager:info("receive fax for faxbox: ~s", [FaxboxId]),
    Props = props:filter_undefined(
              props:filter_empty([{<<"Call">>, kapps_call:to_json(Call)}
                                 ,{<<"Action">>, <<"receive">>}
                                 ,{<<"FaxBox-ID">>, FaxboxId}
                                 ,{<<"Fax-T38-Option">>, lookup_fax_option(Call, Data)}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ])),
    kapi_fax:publish_req(Props),
    cf_exe:control_usurped(Call).

-spec lookup_fax_option(kapps_call:call(), kz_json:object()) -> kz_term:ne_binary().
lookup_fax_option(Call, Data) ->
    FaxBoxId = get_faxbox_id(Data),
    DefaultFaxBoxOption = case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), FaxBoxId) of
                              {'ok', JObj} -> kz_json:get_value(?FAX_OPTION, JObj);
                              _ -> 'undefined'
                          end,
    kz_json:get_value(?FAX_OPTION, Data, DefaultFaxBoxOption).

-spec get_faxbox_id(kz_json:object()) -> kz_term:api_binary().
get_faxbox_id(JObj) ->
    case kz_json:get_ne_binary_value(<<"id">>, JObj) of
        'undefined' -> kz_json:get_ne_binary_value(<<"faxbox_id">>, JObj);
        Id -> Id
    end.
