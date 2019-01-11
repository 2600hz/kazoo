%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_bindings).

-export([start_link/0]).
-export([bind/3, bind/4
        ,flush_mod/1
        ,notification/1
        ]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).
-define(ROUTING_KEY(Category, Name), <<"teletype.", Category/binary, ".", Name/binary>>).

-spec start_link() -> startlink_ret().
start_link() ->
    start_modules(),
    'ignore'.

-spec bind(ne_binary(), module(), atom()) -> 'ok'.
-spec bind(api_binary(), api_binary(), module(), atom()) -> 'ok'.
bind(EventName, Module, Fun) ->
    bind(<<"notification">>, EventName, Module, Fun).
bind(EventCategory, EventName, Module, Fun) ->
    kazoo_bindings:bind(?ROUTING_KEY(EventCategory, EventName), Module, Fun).

-spec flush_mod(module()) -> 'ok'.
flush_mod(Module) ->
    kazoo_bindings:flush_mod(Module).

-spec notification(kz_json:object()) -> 'ok'.
notification(JObj) ->
    kz_util:put_callid(JObj),
    {EventCategory, EventName} = kz_util:get_event_type(JObj),
    ShouldHandle = teletype_util:should_handle_notification(JObj),
    RoutingKey = ?ROUTING_KEY(EventCategory, EventName),
    maybe_handle_notification(JObj, RoutingKey, ShouldHandle).

-spec maybe_handle_notification(kz_json:object(), ne_binary(), boolean()) -> 'ok'.
maybe_handle_notification(_, _, 'false') ->
    'ok';
maybe_handle_notification(JObj, RoutingKey, 'true') ->
    lager:debug("dispatching notification ~s", [RoutingKey]),
    Res = kazoo_bindings:map(RoutingKey, JObj),
    case kazoo_bindings:succeeded(Res, fun filter_out_failed/1) of
        [] ->
            FailureMsg = build_failure_message(Res),
            lager:debug("notification ~s did not result in successes: ~s", [RoutingKey, FailureMsg]),
            teletype_util:send_update(JObj, <<"failed">>, FailureMsg);
        Successes ->
            lager:debug("notification ~s result in ~b success(es)", [RoutingKey, length(Successes)])
    end.

-spec start_modules() -> 'ok'.
start_modules() ->
    start_modules(?AUTOLOAD_MODULES).

-spec start_modules([module()]) -> 'ok'.
start_modules([Module | Remaining]) ->
    teletype_maintenance:start_module(Module),
    start_modules(Remaining);
start_modules([]) ->
    lager:info("started all teletype modules").


-spec filter_out_failed(any()) -> boolean().
filter_out_failed('ok') -> 'true';
filter_out_failed(_) -> 'false'.

-spec build_failure_message(any()) -> ne_binary().
build_failure_message([{'EXIT', {'error', 'missing_data',  Missing}}|_]) ->
    <<"missing_data: ", (kz_term:to_binary(Missing))/binary>>;

build_failure_message([{'EXIT', {'error', 'failed_template',  ModuleName}}|_]) ->
    %% teletype_templates:build_renderer, probably it's only for teletype startup
    <<"failed_template: ", (kz_term:to_binary(ModuleName))/binary>>;

build_failure_message([{'EXIT',{'error', 'template_error',  Reason}}|_]) ->
    <<"template_error: ", (kz_term:to_binary(Reason))/binary>>;

build_failure_message([{'EXIT', {'function_clause', _ST}}|_]) ->
    <<"template_error: crashed with function_clause">>;

build_failure_message([{'EXIT', {'undef', _ST}}|_]) ->
    <<"template_error: crashed with undef">>;

build_failure_message([{'EXIT', {'error', {'badmatch',  _}}}|_]) ->
    %% Some templates (like voicemail_new) is matching against successful open_doc
    %% maybe because the document or attachment is not stored yet.
    %% Let the publisher save the payload to load later
    <<"badmatch">>;

build_failure_message([{'EXIT', {'error', Reason}}|_]) ->
    try kz_term:to_binary(Reason)
    catch _:_ -> <<"unknown error throw-ed">>
    end;

build_failure_message([{'EXIT', {_Exp, _ST}}|_]) ->
    <<"template_error: crashed with exception">>;

build_failure_message([]) ->
    <<"no teletype template modules responded">>;

build_failure_message(_Other) ->
    lager:debug("template failed with unknown reasons: ~p", [_Other]),
    <<"unknown_template_error">>.
