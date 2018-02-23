%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Ben Partridge
%%%-------------------------------------------------------------------
-module(nv_apns).

-behaviour(gen_server).

-include("navi.hrl").

-define(DEV_HOST, "api.development.push.apple.com").
-define(DEV_PORT, 443).
-define(PROD_HOST, "api.push.apple.com").
-define(PROD_PORT, 443).

-export([start_link/2
        ,push/3, push/4
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {name   :: atom()
               ,topic  :: kz_term:ne_binary()
               ,pid    :: pid()
               }).
-type state() :: #state{}.

-type init_retval() :: {'ok', state()} | {'stop', any()}.
-type partial_connection()   :: #{name       => apns_connection:name()
                                 ,certdata   => binary()
                                 ,keydata    => {'RSAPrivateKey', binary()}
                                 ,timeout    => integer()
                                 ,type       => apns_connection:type()
                                 }.


-spec start_link(atom(), kz_json:object()) -> kz_types:startlink_ret().
start_link(MyName, ServerConfig) ->
    lager:debug("Starting apns notification server: ~p", [MyName]),
    RetVal = gen_server:start_link({'local', MyName}, ?MODULE, [MyName, ServerConfig],[]),
    RetVal.

-spec init(any()) -> init_retval().
init([MyName, ServerConfig]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(MyName),
    Name = kz_term:to_atom(kz_term:to_binary(io_lib:format("~s_srv", [MyName])), 'true'),
    PemDecodedCert = public_key:pem_decode(kz_json:get_value(<<"certificate">>, ServerConfig)),
    [{'Certificate', Certificate, _}|_] = PemDecodedCert,
    KeyPem = kz_json:get_value(<<"key">>, ServerConfig),
    PemDecodedKey = public_key:pem_decode(KeyPem),
    [{'RSAPrivateKey', Key, _}|_] = PemDecodedKey,
    Connection = #{name       => Name
                  ,certdata   => Certificate
                  ,keydata    => {'RSAPrivateKey', Key}
                  ,timeout    => 10000
                  ,type       => 'certdata'
                  },
    Topic = kz_json:get_value(<<"default_topic">>, ServerConfig),
    lager:debug("starting apns push notification server: ~p", [Name]),
    init_apns_connection(kz_json:get_value(<<"environment">>, ServerConfig), Connection, Name, Topic).

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', {RegistrationId, Message, ExtraParams}}, #state{name=Name, topic=Topic}=State) ->
    lager:debug("Received request to push notification into apns"),
    TrueTopic = get_true_topic(Topic, ExtraParams),
    Headers = #{ 'apns_topic' => TrueTopic
               },
    Notification = #{aps => #{alert => Message}
                    ,metadata => kz_json:to_map(props:get_value(<<"metadata">>, ExtraParams))},
    case apns:push_notification(Name, RegistrationId, Notification, Headers) of
        {'timeout', _StreamId} ->
            lager:info("apns notification timed out in connection"),
            {'noreply', State};
        {200, _, _} ->
            lager:debug("apns notification sent successfully"),
            {'noreply', State};
        {_, [{<<"apns-id">>, Id}], Err} ->
            lager:error("Error delivering notification: ~s - ~p", [Id, Err]),
            {'noreply', State};
        _ ->
            {'noreply', State}
    end;
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{pid=Pid}) ->
    lager:info("nv_apns terminating with reason: ~p. Destroying apns connection", [Reason]),
    apns:close_connection(Pid),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
push(Srv, RegistrationId, Message) ->
    push(Srv, RegistrationId, Message, []).
-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
push(Srv, RegistrationId, Message, ExtraParams) ->
    gen_server:cast(Srv, {'push', {RegistrationId, Message, ExtraParams}}).

%%=========================================================
%%                 Private Methods
%%========================================================
-spec init_apns_connection(kz_term:ne_binary(), partial_connection(), atom(), kz_term:ne_binary()) -> init_retval().
init_apns_connection(<<"dev">>, Connection, Name, Topic) ->
    lager:debug("Creating dev apns connection: ~p", [Name]),
    NewConnection = Connection#{apple_host => ?DEV_HOST, apple_port => ?DEV_PORT},
    {'ok', Pid} = apns:connect(NewConnection),
    {'ok', #state{name=Name, topic=Topic, pid=Pid}};

init_apns_connection(<<"prod">>, Connection, Name, Topic) ->
    lager:debug("Creating prod apns connection: ~p", [Name]),
    NewConnection = Connection#{apple_host => ?PROD_HOST, apple_port => ?PROD_PORT},
    {'ok', Pid} = apns:connect(NewConnection),
    {'ok', #state{name=Name, topic=Topic, pid=Pid}}.

-spec get_true_topic(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary().
get_true_topic(Topic, ExtraParameters) ->
    case props:get_value(<<"topic_extension">>, ExtraParameters) of
        <<"voip">> -> kz_term:to_binary(io_lib:format(<<"~p.voip">>, [Topic]));
        <<"complication">> -> kz_term:to_binary(io_lib:format(<<"~p.complication">>, [Topic]));
        'undefined' -> Topic
    end.
