%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% EDR in-core API
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(kz_edr).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-export([event/5, event/6]).

-type event_return() :: 'ok' |
                            {'error', any()} |
                            {'returned', kz_json:object(), kz_json:object()}.

-spec event(binary(), binary(), edr_severity(), edr_verbosity(), kz_json:object()) -> event_return().
-spec event(binary(), binary(), edr_severity(), edr_verbosity(), kz_json:object(), api_binary()) -> event_return().
event(AppName, AppVersion, Severity, Verbosity, Body) ->
    event(AppName, AppVersion, Severity, Verbosity, Body, 'undefined').
event(AppName, AppVersion, Severity, Verbosity, Body, AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Body">>, Body}
          ,{<<"ID">>, kz_datamgr:get_uuid()}
          ,{<<"Severity">>, Severity}
          ,{<<"Timestamp">>, kz_time:now_s(kz_time:now())}
          ,{<<"Verbosity">>, Verbosity}
           | kz_api:default_headers(<<"edr">>, <<"event">>, AppName, AppVersion)
          ],
    kz_amqp_worker:cast(Req, fun kapi_edr:publish/1).
