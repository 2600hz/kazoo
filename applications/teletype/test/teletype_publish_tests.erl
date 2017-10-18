-module(teletype_publish_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

-define(TEST_POOL_ARGS, [{worker_module, teletype_renderer}
                        ,{name, {local, teletype_render_farm}}
                        ,{size, 1}
                        ,{max_overflow, 1}
                        ]).

-spec teletype_publish_test_() -> any().
teletype_publish_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_) -> [{"Testing kapps_notify_publisher and Teletype responses", test_notify_publisher()}] end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Teletype publish test"),
    mock_them_all(),

    {ok, _} = application:ensure_all_started(kazoo_bindings),
    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    lager:set_loglevel(lager_console_backend, none),
    lager:set_loglevel(lager_file_backend, none),
    lager:set_loglevel(lager_syslog_backend, none),

    ok = application:ensure_started(poolboy),
    {ok, BoyPid} = poolboy:start_link(?TEST_POOL_ARGS),
    ignore = teletype_bindings:start_link(),

    {LinkPid, BoyPid}.

cleanup({LinkPid, BoyPid}) ->
    validate_mock(),
    stop_data_link(LinkPid),
    ok = poolboy:stop(BoyPid),
    _ = [application:stop(App) || App <- [kazoo_bindings, kazoo_etsmgr, lager, poolboy]],
    meck:unload().

stop_data_link(Pid) ->
    _DataLink = erlang:exit(Pid, normal),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.

test_notify_publisher() ->
    [{"Test publishing " ++ binary_to_list(Id) ++ " notification"
     ,pulish_notification(Id, PubFun)
     }
     || {Id, PubFun} <- [{teletype_voicemail_to_email:id(), fun kapi_notifications:publish_voicemail_new/1}
                        ,{teletype_deregister:id(), fun kapi_notifications:publish_deregister/1}
                        ]
    ].

pulish_notification(TemplateId, PubFun) ->
    TemplateIdStr = binary_to_list(TemplateId),
    Path = "fixtures-api/notifications/" ++ TemplateIdStr ++ ".json",
    {ok, Payload} = kz_json:fixture(kazoo_amqp, Path),
    ?_assertEqual(<<"completed">>, get_status(kapps_notify_publisher:call_collect(Payload, PubFun))).

get_status([]) -> false;
get_status([JObj|_]) ->
    kz_json:get_ne_binary_value(<<"Status">>, JObj);
get_status({_, JObjs}) when is_list(JObjs) ->
    get_status(JObjs);
get_status({_, JObj}) ->
    get_status(JObj).

mock_them_all() ->
    meck:new(kz_amqp_worker, [unstick, passthrough]),
    meck:new(amqp_util, [unstick, passthrough]),
    meck:new(gen_smtp_client, [unstick, passthrough]),

    meck:expect(kz_amqp_worker, call_collect, kz_amqp_worker_call_collect()),
    meck:expect(kz_amqp_worker, cast, kz_amqp_worker_cast()),
    meck:expect(amqp_util, targeted_publish, amqp_util_targeted_publish()),
    meck:expect(amqp_util, notifications_publish, amqp_util_notifications_publish()),
    meck:expect(gen_smtp_client, send, smtp_send()).

validate_mock() ->
    meck:validate(kz_amqp_worker),
    meck:validate(amqp_util),
    meck:validate(gen_smtp_client).

%% call by kapps_notify_publisher:call_collect/2 and kapps_notify_publisher:cast/2
kz_amqp_worker_call_collect() ->
    fun(Req, PublishFun, UntilFun, Timeout) ->
            try PublishFun(Req) of
                ok -> wait_collect_until(UntilFun, [], erlang:start_timer(Timeout, self(), req_timeout))
            catch
                _E:T ->
                    ?LOG_DEBUG("failed to publish: ~p:~p", [_E, T]),
                    kz_util:log_stacktrace(),
                    {error, T}
            end
    end.
wait_collect_until(UntilFun, Resps, ReqRef) ->
    receive
        {timeout, ReqRef, req_timeout} ->
            ?LOG_DEBUG("wait_collect_until timeout"),
            {timeout, Resps};
        {ok, Resp} ->
            Responses = [Resp|Resps],
            try UntilFun(Responses) of
                true ->
                    _ = erlang:cancel_timer(ReqRef),
                    {ok, Responses};
                false ->
                    wait_collect_until(UntilFun, Responses, ReqRef)
            catch
                _E:_R ->
                    ?LOG_DEBUG("supplied until_fun crashed: ~s: ~p", [_E, _R]),
                    ?LOG_DEBUG("pretending like until_fun returned false"),
                    wait_collect_until(UntilFun, Responses, ReqRef)
            end
    after 2000 ->
            ?LOG_DEBUG("hard timeout"),
            {error, timeout}
    end.


%% call by kapi_notifications:publish_*/1
amqp_util_notifications_publish() ->
    fun(_, Payload, _) ->
            _ = teletype_bindings:notification(kz_json:decode(Payload)),
            %% line above actually has the result, but it's single response and
            %% we want to collect more responses so naively returning ok here to
            %% setup message passing between imaginary teletype process
            %% and imaginary amqp worker
            ok
    end.

%% call by teletype_util:send_update/2,3,4
kz_amqp_worker_cast() ->
    fun(Prop, PublishFun) ->
            PublishFun(Prop)
    end.

%% call by kapi_notifications:publish_notify_update/2
amqp_util_targeted_publish() ->
    fun(_, Payload, _) ->
            %% if we could have the eunit test's pid, and spawn kapi_notifications:publish_* above then
            %% we have a real process for teletype which is sending messages to mocked qmqp worker above.
            %% But I couldn't set it up properly, it seems meck is jumping in the middle
            %% and spawning another process for mocking this function or something like that?
            self() ! {ok, kz_json:decode(Payload)}
    end.

smtp_send() ->
    fun({_, _, _}, _, CallBack) -> CallBack({ok, <<"Message accepted">>}) end.
