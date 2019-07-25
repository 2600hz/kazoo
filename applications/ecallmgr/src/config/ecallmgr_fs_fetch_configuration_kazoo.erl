%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_kazoo).

%% API
-export([init/0]).

-export([kazoo/1]).

-export([kazoo_config/0]).

-import(ecallmgr_fs_xml
       ,[section_el/2
        ,config_el/3
        ,xml_attrib/2
        ]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Initializes the bindings
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"fetch.configuration.community.*.kazoo.conf">>, ?MODULE, 'kazoo'),
    'ok'.

-spec kazoo(map()) -> fs_sendmsg_ret().
kazoo(#{node := Node, fetch_id := Id, payload := JObj} = Ctx) ->
    kz_util:put_callid(Id),
    lager:debug("received configuration request for kazoo configuration ~p , ~p", [Node, Id]),
    fs_mod_kazoo_config(kz_api:event_name(JObj), Ctx).

-spec fs_mod_kazoo_config(kz_term:ne_binary(), map()) -> fs_sendmsg_ret().
fs_mod_kazoo_config(<<"COMMAND">>, #{payload := _JObj} = Ctx) ->
    lager:debug_unsafe("kazoo conf request : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    kazoo_req_not_handled(Ctx);
fs_mod_kazoo_config(<<"REQUEST_PARAMS">>, #{payload := JObj} = Ctx) ->
    lager:debug_unsafe("kazoo conf request params: ~s", [kz_json:encode(JObj, ['pretty'])]),
    Action = kz_json:get_ne_binary_value(<<"Action">>, JObj),
    fs_mod_kazoo_config_action(Action, Ctx);
fs_mod_kazoo_config(Event, #{node := Node} = Ctx) ->
    lager:debug("unhandled mod kazoo config event : ~p : ~p", [Node, Event]),
    kazoo_req_not_handled(Ctx).

-spec fs_mod_kazoo_config_action(kz_term:api_ne_binary(), map()) -> fs_sendmsg_ret().
fs_mod_kazoo_config_action(<<"request-handlers">>, Ctx) ->
    try kazoo_config() of
        {'ok', Xml} -> freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(Xml)})
    catch
        _Ex:_Er:ST ->
            kz_util:log_stacktrace(ST),
            kazoo_req_not_handled(Ctx)
    end;
fs_mod_kazoo_config_action('undefined', Ctx) ->
    kazoo_req_not_handled(Ctx);
fs_mod_kazoo_config_action(Action, #{node := Node} = Ctx) ->
    lager:debug("unhandled mod kazoo config action : ~p : ~p", [Node, Action]),
    kazoo_req_not_handled(Ctx).

-spec kazoo_req_not_handled(map()) -> fs_sendmsg_ret().
kazoo_req_not_handled(#{node := Node, fetch_id := Id} = Ctx) ->
    {'ok', NotHandled} = ecallmgr_fs_xml:not_found(),
    lager:debug("ignoring kazoo conf ~s: ~s", [Node, Id]),
    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(NotHandled)}).

-spec kazoo_config() -> {'ok', iolist()}.
kazoo_config() ->
    EventFiles = filelib:wildcard(code:priv_dir(?APP) ++ "/mod_kazoo/events/*.xml"),
    {DefFiles0, Events} = lists:foldr(fun fs_handler/2, {[], []}, EventFiles),

    FetchFiles = filelib:wildcard(code:priv_dir(?APP) ++ "/mod_kazoo/fetch/*.xml"),
    {DefFiles, FetchProfiles} = lists:foldr(fun fs_handler/2, {DefFiles0, []}, FetchFiles),

    Defs0 = lists:foldl(fun one_def/2, [], DefFiles),
    Defs = lists:map(fun fs_xml/1, Defs0),

    ProfileEl = event_profile_el("default", events_el(Events)),

    ConfigurationEl = config_el(<<"kazoo.conf">>, <<"Built by Kazoo">>
                               ,[definitions_el(Defs)
                                ,event_handlers_el([ProfileEl])
                                ,fetch_handlers_el(FetchProfiles)
                                ]
                               ),
    SectionEl = section_el(<<"configuration">>, ConfigurationEl),
    {'ok', xmerl:export([SectionEl], 'fs_xml')}.

fs_handler(EventFile, {DefFiles, EventXmls}) ->
    EventXml = fs_xml(EventFile),
    {fs_defs(EventXml, DefFiles), [EventXml | EventXmls]}.

-spec fs_defs(kz_types:xml_el(), kz_types:xml_els()) -> kz_types:xml_els().
fs_defs(XmlEl, Acc) ->
    RefFileList = lists:map(fun fs_def_filename/1, xmerl_xpath:string("//field[@type='reference']/@name", XmlEl)),
    RefXmls = lists:map(fun fs_xml/1, RefFileList),
    lists:foldl(fun fs_defs/2, [], RefXmls) ++ RefFileList ++ Acc.

-spec fs_xml(file:filename_all()) -> kz_types:xml_el().
fs_xml(File) ->
    {Xml, _} = xmerl_scan:file(re:replace(File, "::", "-", ['global'])),
    Xml.

-spec fs_def_filename(kz_types:xml_attrib() | string()) -> file:filename_all().
fs_def_filename(#xmlAttribute{name='name', value=Name}) ->
    fs_def_filename(Name);
fs_def_filename(Name) ->
    code:priv_dir(?APP) ++ "/mod_kazoo/definitions/" ++ Name ++ ".xml".

-spec one_def(file:filename_all(), [file:filename_all()]) -> [file:filename_all()].
one_def(File, Acc) ->
    case lists:member(File, Acc) of
        'true' -> Acc;
        'false' -> Acc ++ [File]
    end.

-spec definitions_el(kz_types:xml_els()) -> kz_type:xml_el().
definitions_el(Content) ->
    #xmlElement{name='definitions'
               ,content=Content
               }.

-spec events_el(kz_types:xml_els()) -> kz_type:xml_el().
events_el(Content) ->
    #xmlElement{name='events'
               ,content=Content
               }.

-spec event_profile_el(string(), kz_types:xml_el()) -> kz_type:xml_el().
event_profile_el(Name, Content) ->
    #xmlElement{name='profile'
               ,attributes=[xml_attrib('name', Name)]
               ,content=[Content]
               }.

-spec event_handlers_el(kz_types:xml_els()) -> kz_type:xml_el().
event_handlers_el(Content) ->
    #xmlElement{name='event-handlers'
               ,content=Content
               }.

-spec fetch_handlers_el(kz_types:xml_els()) -> kz_type:xml_el().
fetch_handlers_el(Content) ->
    #xmlElement{name='fetch-handlers'
               ,content=Content
               }.
