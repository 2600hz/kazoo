%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% EDR in-core API
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%    Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(kz_edr).

-include_lib("../include/edr.hrl").

-export([event/5, event/6]).

%% Callback for new process
-export([send_event/1]).

-type event_return() :: 'ok' |
                        {'error', any()} |
                        {'returned', kz_json:object(), kz_json:object()}.

-spec event(binary(), binary(), edr_severity(), edr_verbosity(), kz_json:object()) -> event_return().
-spec event(binary(), binary(), edr_severity(), edr_verbosity(), kz_json:object(), api_binary()) -> event_return().
event(AppName, AppVersion, Severity, Verbosity, Body) ->
    event(AppName, AppVersion, Severity, Verbosity, Body, 'undefined').
event(AppName, AppVersion, Severity, Verbosity, Body, AccountId) ->
    GregorianTime = kz_time:now_s(kz_time:now()),
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
                      ,verbosity=Verbosity},
    %% We want to resume execution as soon as possible, and not to crash on event processing failure.
    %% So, we'll spawn a new process to do this
    spawn(?MODULE, 'send_event', [Event]),
    'ok'.

-spec send_event(edr_event()) -> any().
send_event(#edr_event{account_id=AccountId}=Event) ->
    edr_bindings:distribute(Event#edr_event{account_tree=kz_util:account_tree(AccountId)}).
