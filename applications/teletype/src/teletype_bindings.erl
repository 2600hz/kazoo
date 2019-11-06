%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_bindings).

-export([start_link/0]).
-export([bind/3, bind/4
        ,flush_mod/1
        ,notification/1
        ]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).
-define(ROUTING_KEY(Category, Name), <<"teletype.", Category/binary, ".", Name/binary>>).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    start_modules(),
    garbage_collect(self()),
    'ignore'.

-spec bind(kz_term:ne_binary(), module(), atom()) -> 'ok'.
bind(EventName, Module, Fun) ->
    bind(<<"notification">>, EventName, Module, Fun).

-spec bind(kz_term:api_binary(), kz_term:api_binary(), module(), atom()) -> 'ok'.
bind(EventCategory, EventName, Module, Fun) ->
    Binding = ?ROUTING_KEY(EventCategory, EventName),
    case kazoo_bindings:bind(Binding, Module, Fun) of
        'ok' -> lager:debug("~s has been bound", [Binding]);
        {'error', 'exists'} -> lager:debug("bind for ~s exists", [Binding])
    end.

-spec flush_mod(module()) -> 'ok'.
flush_mod(Module) ->
    kazoo_bindings:flush_mod(Module).

-spec start_modules() -> 'ok'.
start_modules() ->
    start_modules(?AUTOLOAD_MODULES).

-spec start_modules([module()]) -> 'ok'.
start_modules(Modules) ->
    lists:foreach(fun teletype_maintenance:start_module/1, Modules),
    lager:info("started all teletype modules").

-spec notification(kz_json:object()) -> 'ok'.
notification(JObj) ->
    kz_log:put_callid(JObj),
    {EventCategory, EventName} = kz_util:get_event_type(JObj),
    ShouldHandle = teletype_util:should_handle_notification(JObj),
    RoutingKey = ?ROUTING_KEY(EventCategory, EventName),
    maybe_handle_notification(JObj, RoutingKey, ShouldHandle).

-spec maybe_handle_notification(kz_json:object(), kz_term:ne_binary(), boolean()) -> 'ok'.
maybe_handle_notification(_, _, 'false') ->
    'ok';
maybe_handle_notification(JObj, RoutingKey, 'true') ->
    lager:debug("dispatching notification ~s", [RoutingKey]),
    case kazoo_bindings:map(RoutingKey, JObj) of
        [] ->
            FailureMsg = <<"no teletype template modules responded to notification ", RoutingKey/binary>>,
            lager:debug("~s", [FailureMsg]),
            teletype_util:send_update(JObj, <<"failed">>, FailureMsg);
        BindingResult ->
            {_, Map} = lists:foldl(fun check_result/2, {RoutingKey, maps:new()}, BindingResult),
            maybe_send_update(JObj, RoutingKey, Map)
    end.

-spec maybe_send_update(kz_json:object(), kz_term:ne_binary(), map()) -> 'ok'.
maybe_send_update(JObj, RoutingKey, #{'completed' := _Completed}=Map) ->
    %% for now we just only care about at least one success
    print_result(RoutingKey, Map),
    Metadata = kz_json:from_list_recursive(maps:to_list(Map)),
    teletype_util:send_update(JObj, <<"completed">>, 'undefined', Metadata);
maybe_send_update(JObj, RoutingKey, #{'failed' := [{_, Reason}|_]}=Map) ->
    %% for now just send the first error as failure message
    print_result(RoutingKey, Map),
    Metadata = kz_json:from_list_recursive(maps:to_list(Map)),
    teletype_util:send_update(JObj, <<"failed">>, Reason, Metadata);
maybe_send_update(JObj, RoutingKey, #{'disabled' := _Completed}=Map) ->
    %% for now just send the first disabled as failure message
    print_result(RoutingKey, Map),
    Metadata = kz_json:from_list_recursive(maps:to_list(Map)),
    teletype_util:send_update(JObj, <<"disabled">>, 'undefined', Metadata);
maybe_send_update(JObj, RoutingKey, #{'ignored' := _Completed}=Map) ->
    %% for now just send the first ignored as failure message
    print_result(RoutingKey, Map),
    Metadata = kz_json:from_list_recursive(maps:to_list(Map)),
    teletype_util:send_update(JObj, <<"ignored">>, 'undefined', Metadata);
maybe_send_update(JObj, RoutingKey, Map) ->
    print_result(RoutingKey, Map),
    Metadata = kz_json:from_list_recursive(maps:to_list(Map)),
    teletype_util:send_update(JObj, <<"completed">>, 'undefined', Metadata).

-spec print_result(kz_term:ne_binary(), map()) -> 'ok'.
print_result(RoutingKey, Map) ->
    Completed = erlang:length(maps:get('completed', Map, [])),
    Disabled = erlang:length(maps:get('disabled', Map, [])),
    Ignored = erlang:length(maps:get('ignored', Map, [])),
    Failed = erlang:length(maps:get('failed', Map, [])),
    ?LOG_DEBUG("notification ~s resulted in ~b success, ~b failed, ~b ignored, ~b disabled, full result: ~1000p"
              ,[RoutingKey, Completed, Failed, Ignored, Disabled, maps:to_list(Map)]
              ).

-spec check_result(any(), {kz_term:ne_binary(), map()}) -> {kz_term:ne_binary(), map()}.

check_result('ok', {RoutingKey, Map}) ->
    {RoutingKey, maps:update_with('completed', update_with(RoutingKey), [RoutingKey], Map)};

check_result({'completed', TemplateId}, {RoutingKey, Map}) ->
    {RoutingKey, maps:update_with('completed', update_with(TemplateId), [TemplateId], Map)};

check_result({'ignored', TemplateId}, {RoutingKey, Map}) ->
    {RoutingKey, maps:update_with('ignored', update_with(TemplateId), [TemplateId], Map)};

check_result({'disabled', TemplateId}, {RoutingKey, Map}) ->
    {RoutingKey, maps:update_with('disabled', update_with(TemplateId), [TemplateId], Map)};

check_result({'failed', Reason, TemplateId}, {RoutingKey, Map}) ->
    {RoutingKey, maps:update_with('failed', update_with({TemplateId, Reason}), [{TemplateId, Reason}], Map)};

check_result({'EXIT', {'error', 'missing_data',  Missing}}, {RoutingKey, Map}) ->
    Reason = <<"missing_data: ", (kz_term:to_binary(Missing))/binary>>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT', {'error', 'failed_template',  ModuleName}}, {RoutingKey, Map}) ->
    %% teletype_templates:build_renderer, probably it's only for teletype startup
    Reason = <<"failed_template: ", (kz_term:to_binary(ModuleName))/binary>>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT',{'error', 'template_error',  Error}}, {RoutingKey, Map}) ->
    Reason = <<"template_error: ", (kz_term:to_binary(Error))/binary>>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT', {'function_clause', _ST}}, {RoutingKey, Map}) ->
    Reason = <<"template_error: crashed with function_clause">>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT', {'undef', _ST}}, {RoutingKey, Map}) ->
    Reason = <<"template_error: crashed with undef">>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT', {'error', {'badmatch',  _}}}, {RoutingKey, Map}) ->
    %% Some templates (like voicemail_new) is matching against successful open_doc
    %% If it failed due to the document or attachment is not stored yet or db timeout
    %% let the publisher save the payload
    Reason = <<"badmatch">>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result({'EXIT', {'error', Reason}}, {RoutingKey, Map}) ->
    ReasonBin = try kz_term:to_binary(Reason)
                catch _:_ -> <<"unknown error throw-ed">>
                end,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, ReasonBin}), [{RoutingKey, ReasonBin}], Map)};

check_result({'EXIT', {_Exp, _ST}}, {RoutingKey, Map}) ->
    Reason = <<"template_error: crashed with exception">>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)};

check_result(_Other, {RoutingKey, Map}) ->
    Reason = <<"unknown_template_error">>,
    {RoutingKey, maps:update_with('failed', update_with({RoutingKey, Reason}), [{RoutingKey, Reason}], Map)}.

update_with(Value) -> fun(ResultList) -> [Value | ResultList] end.
