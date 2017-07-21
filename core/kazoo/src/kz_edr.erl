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

-export([log_event/5]).

-type log_event_return() :: 'ok' |
                            {'error', any()} |
                            {'returned', kz_json:object(), kz_json:object()}.

-spec log_event(binary(), binary(), kz_json:object(), binary(), binary()) -> log_event_return().
log_event(EventCategory, EventName, Tags, AppName, AppVersion) ->
    Timestamp = kz_time:now_s(kz_time:now()),
    Req = [{<<"Timestamp">>, Timestamp}
          ,{<<"Tags">>, Tags}
           | kz_api:default_headers(EventCategory, EventName, AppName, AppVersion)
          ],
    kz_amqp_worker:cast(Req, fun kapi_edr:publish/1).
