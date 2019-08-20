%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc EDR in-core API
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @author Conversant Ltd (Max Lay)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_edr).

-include_lib("../include/edr.hrl").

-export([event/5, event/6]).

%% Callback for new process
-export([send_event/1]).

-spec event(kz_term:ne_binary(), kz_term:ne_binary(), edr_severity(), edr_verbosity(), kz_json:object()) -> 'ok'.
event(AppName, AppVersion, Severity, Verbosity, Body) ->
    event(AppName, AppVersion, Severity, Verbosity, Body, 'undefined').

-spec event(kz_term:ne_binary(), kz_term:ne_binary(), edr_severity(), edr_verbosity(), kz_json:object(), kz_term:api_ne_binary()) -> 'ok'.
event(AppName, AppVersion, Severity, Verbosity, Body, AccountId) ->
    GregorianTime = kz_time:now_s(),
    Event = #edr_event{account_id=AccountId
                      ,app_name=AppName
                      ,app_version=AppVersion
                      ,body=Body
                      ,id=kz_datamgr:get_uuid()
                      ,node=kz_term:to_binary(node())
                      ,severity=Severity
                      ,timestamp=kz_time:iso8601(GregorianTime)
                       %% Time needs to be computed ASAP
                      ,gregorian_time=GregorianTime
                      ,verbosity=Verbosity
                      },
    %% We want to resume execution as soon as possible, and not to crash on event processing failure.
    %% So, we'll spawn a new process to do this
    spawn(?MODULE, 'send_event', [Event]),
    'ok'.

-spec send_event(edr_event()) -> any().
send_event(#edr_event{account_id=AccountId}=Event) ->
    {'ok', Account} = kzd_accounts:fetch(AccountId),
    edr_bindings:distribute(Event#edr_event{account_tree=kzd_accounts:tree(Account)}).
