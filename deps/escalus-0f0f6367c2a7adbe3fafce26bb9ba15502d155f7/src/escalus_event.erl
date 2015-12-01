-module(escalus_event).

%% Installation/deinstallation of the event mgr
-export([start/1,
         stop/1,
         new_client/3,
         add_handler/3,
         delete_handler/3]).

%% Notifications
-export([incoming_stanza/2,
         outgoing_stanza/2,
         pop_incoming_stanza/2,
         story_start/1,
         story_end/1]).

%% History
-export([get_history/1,
         print_history/1]).

-export_type([event_client/0]).

-include_lib("exml/include/exml.hrl").
-include("no_binary_to_integer.hrl").

-type config() :: escalus_config:config().
-type event_client() :: list({atom(), any()}).
-type manager() :: pid().
-type resource() :: binary().

%% =======================================================================

%% @doc Add an event handler
%% @end
-spec add_handler(manager(), atom() | pid(), [term()]) -> ok.
add_handler(Mgr, Handler, Args) ->
    gen_event:add_handler(Mgr, Handler, Args).

%% @doc Delete an event handler
%% @end
-spec delete_handler(manager(), atom() | pid(), [term()]) -> ok.
delete_handler(Mgr, Handler, Args) ->
    gen_event:delete_handler(Mgr, Handler, Args).

incoming_stanza(Client, Stanza) ->
    notify_stanza(Client, incoming_stanza, Stanza).

pop_incoming_stanza(Client, Stanza) ->
    notify_stanza(Client, pop_incoming_stanza, Stanza).

outgoing_stanza(Client, Stanza) ->
    notify_stanza(Client, outgoing_stanza, Stanza).

story_start(Config) ->
    gen_event:notify(manager(Config), story_start).

story_end(Config) ->
    gen_event:notify(manager(Config), story_end).


%% ====================================================================

%% @doc Start the event manager
%% @end
start(Config) ->
    {ok, Mgr} = gen_event:start_link(),
    add_handler(Mgr, escalus_history_h, []),
    [{escalus_event_mgr, Mgr} | Config].

%% @doc Stop the event manager
%% @end
stop(Config) ->
    gen_event:stop(manager(Config)),
    Config.

get_history(Config) ->
    escalus_history_h:get_history(manager(Config)).

print_history(Config) ->
    CaseName = proplists:get_value(tc_name, Config),
    PrivDir  = proplists:get_value(priv_dir, Config),
    FileName = atom_to_list(CaseName) ++ ".xml",
    FullFileName = filename:join(PrivDir, FileName),
    Events = get_history(Config),
    write_events(Events, FullFileName),
    % escalus_ct:add_log_link(Heading, FileName, Type),
    escalus_ct:add_log_link("history.xml", FileName, ""),
    ok.

write_events([], _) ->
    ok;
write_events(Events, OutFileName) ->
    {ok, FD} = file:open(OutFileName, [write]),
    BaseTime = base_time(Events),
    file:write(FD, <<"<history xmlns:stream=\""
                     "http://etherx.jabber.org/streams\">">>),
    [ file:write(FD, exml:to_iolist(build_elem_event(BaseTime, E)))
      ||  E <- collapse_incoming_events(filter_elements(Events)) ],
    file:write(FD, <<"</history>">>),
    file:close(FD).

filter_elements(Events) ->
    [E || E <- Events, not is_event_with_stream_border(E)].

is_event_with_stream_border({stanza, _Type, _JID, _Time, Elem}) ->
    not is_element(Elem);
is_event_with_stream_border(_) ->
    false.

is_element(#xmlel{}) -> true;
is_element(_)        -> false. %% xmlstreamstart and end

base_time([H|_]) ->
    event_time(H).

event_time({stanza, _Type, _JID, Time, _Elem}) ->
    Time;
event_time({story, _Type, Time}) ->
    Time.

%% @doc Delete a duplicated `Elem'.
collapse_incoming_events([{stanza, incoming_stanza, JID, Time1, Elem},
                          {stanza, pop_incoming_stanza, JID, Time2, Elem}|T]) ->
    [{stanza, incoming_stanza, JID, Time1, Elem},
     {stanza, pop_incoming_stanza, JID, Time2, undefined}
     | collapse_incoming_events(T)];
collapse_incoming_events([H|T]) ->
    [H | collapse_incoming_events(T)];
collapse_incoming_events([]) ->
    [].

build_elem_event(BaseTime, {stanza, Type, JID, Time, Elem}) ->
    build_stanza_event_elem(Type, JID, BaseTime, Time, Elem);
build_elem_event(BaseTime, {story, Type, Time}) ->
    build_story_event_elem(Type, BaseTime, Time).

build_story_event_elem(Type, BaseTime, Time) ->
    #xmlel{name = list_to_binary(atom_to_list(Type)),
           attrs = [{<<"offset">>, time_offset_binary(BaseTime, Time)}]}.

build_stanza_event_elem(Type, JID, BaseTime, Time, Elem) ->
    #xmlel{name = list_to_binary(atom_to_list(Type)),
           attrs = [{<<"jid">>, jid_to_binary(JID)},
                    {<<"offset">>, time_offset_binary(BaseTime, Time)}],
           children = [Elem || Elem =/= undefined]}.

manager(Config) ->
    proplists:get_value(escalus_event_mgr, Config).

%% @doc Create a new event emitter.
-spec new_client(Config, User, MaybeResource) -> undefined | EventClient when
      Config :: config(),
      User :: escalus_users:user_name() | escalus_users:user_spec(),
      MaybeResource :: undefined | resource(),
      EventClient :: event_client().
new_client(Config, User, MaybeResource) when is_list(Config) ->
    UserSpec = escalus_users:get_userspec(Config, User),
    Resource = maybe_resource_to_binary(MaybeResource),
    new_client_1(manager(Config), UserSpec, Resource).

maybe_resource_to_binary(undefined) -> <<>>;
maybe_resource_to_binary(Resource) when is_binary(Resource) -> Resource.

-spec new_client_1(Mgr, UserSpec, Resource) -> undefined | EventClient when
      Mgr :: undefined | manager(),
      UserSpec :: escalus_users:user_spec(),
      Resource :: binary(),
      EventClient :: event_client().
new_client_1(undefined, _UserSpec, _Resource) ->
    undefined;
new_client_1(Mgr, UserSpec, Resource) ->
    [{event_manager, Mgr},
     {server, proplists:get_value(server, UserSpec)},
     {username, proplists:get_value(username, UserSpec)},
     {resource, Resource}].

%% @doc Notify the event system of an event
%% <p>The system accepts any term as the event.</p>
%% @end
notify_stanza(undefined, _, _) ->
    ok;
notify_stanza(Client, EventName, Stanza) ->
    Mgr = proplists:get_value(event_manager, Client),
    Jid = jid(Client),
    gen_event:notify(Mgr, {EventName, Jid, Stanza}).


jid(Client) ->
    Server   = proplists:get_value(server, Client),
    User     = proplists:get_value(username, Client),
    Resource = proplists:get_value(resource, Client),
    {User, Server, Resource}.

jid_to_binary({User, Server, Resource}) ->
    <<User/binary,"@",Server/binary,"/",Resource/binary>>.

now_to_microseconds({Mega, Secs, Micro}) ->
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

time_offset_binary(BaseTime, Time) ->
    Offset = now_to_microseconds(Time) - now_to_microseconds(BaseTime),
    integer_to_binary(Offset).
