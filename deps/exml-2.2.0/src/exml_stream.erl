%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc XML stream parser
%%%
%%% @end
%%% Created : 21 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_stream).

-include("exml_stream.hrl").

-export([new_parser/0,
         new_parser/1,
         parse/2,
         reset_parser/1,
         free_parser/1]).

-export_type([start/0,
              stop/0,
              element/0,
              parser/0,
              parser_opt/0]).

-record(config, {infinite_stream :: boolean(),
                 autoreset :: boolean()}).

-record(parser, {event_parser :: exml_event:c_parser(),
                 config :: parser_cfg(),
                 stack = [] :: list()}).

-type start() :: #xmlstreamstart{}.
-type stop() :: #xmlstreamend{}.
-type element() :: exml:element() | start() | stop().
-type parser_cfg() :: #config{}.
-type parser() :: #parser{}.
%% infinite_stream - no distinct "stream start" or "stream end", only #xmlel{} will be returned
%% autoreset - will reset expat after each parsed document
%%             use only when complete xml document is sent to the parser
%%             for example XMPP over WebSocekts - http://tools.ietf.org/html/draft-ietf-xmpp-websocket
-type parser_property() :: infinite_stream | autoreset.
-type parser_opt() :: {parser_property(), boolean()}.

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new_parser() -> {ok, parser()} | {error, any()}.
new_parser() ->
    new_parser([]).

-spec new_parser([parser_opt()]) -> {ok, parser()} | {error, any()}.
new_parser(Opts)->
    try
        {ok, EventParser} = exml_event:new_parser(),
        {ok, #parser{
            event_parser = EventParser,
            config = #config{
                        infinite_stream = bool_opt(infinite_stream, Opts, false),
                        autoreset = bool_opt(autoreset, Opts, false)}}}
    catch
        E:R -> {error, {E, R}}
    end.

-spec parse(parser(), binary()) ->
        {ok, parser(), [exml_stream:element()]} | {error, {string(), binary()}}.
parse(#parser{event_parser = EventParser, stack = OldStack, config = Config} = Parser, Input) ->
    case exml_event:parse(EventParser, Input) of
        {ok, Events} ->
            {Elements, NewStack} = parse_events(Events, OldStack, [],
                                                Config#config.infinite_stream),
            NewParser = if
                            NewStack =:= [] andalso Config#config.autoreset ->
                                {ok, NewParser0} = reset_parser(Parser),
                                NewParser0;
                            true ->
                                Parser
                        end,
            {ok, NewParser#parser{stack=NewStack}, Elements};
        {error, Msg} ->
            {error, {Msg, Input}}
    end.

-spec reset_parser(parser()) -> {ok, parser()} | {error, any()}.
reset_parser(#parser{event_parser = EventParser, config = Config}) ->
    try
        exml_event:reset_parser(EventParser),
        %% drop all the state except event_parser
        {ok, #parser{event_parser = EventParser, config = Config}}
    catch
        E:R ->
            {error, {E, R}}
    end.

-spec free_parser(parser()) -> ok | {error, any()}.
free_parser(#parser{event_parser = EventParser}) ->
    exml_event:free_parser(EventParser).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec parse_events(list(), list(), list(), boolean()) -> {list(exml_stream:element()), list()}.
parse_events([], Stack, Acc, _InfiniteStream) ->
    {lists:reverse(Acc), Stack};
parse_events([{xml_element_start, Name, NSs, Attrs} | Rest], [], Acc, false) ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    parse_events(Rest, [#xmlel{name = Name, attrs = NewAttrs}],
                 [#xmlstreamstart{name = Name, attrs = NewAttrs} | Acc], false);
parse_events([{xml_element_start, Name, NSs, Attrs} | Rest], Stack, Acc, InfiniteStream) ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    parse_events(Rest, [#xmlel{name = Name, attrs = NewAttrs} | Stack], Acc, InfiniteStream);
parse_events([{xml_element_end, Name} | Rest], [#xmlel{name = Name}], Acc, false) ->
    parse_events(Rest, [], [#xmlstreamend{name = Name} | Acc], false);
parse_events([{xml_element_end, Name} | Rest], [#xmlel{name = Name} = Element], Acc, true) ->
    parse_events(Rest, [], [xml_element(Element) | Acc], true);
parse_events([{xml_element_end, Name} | Rest], [#xmlel{name = Name} = Element, Top], Acc, false) ->
    parse_events(Rest, [Top], [xml_element(Element) | Acc], false);
parse_events([{xml_element_end, _Name} | Rest], [Element, Parent | Stack], Acc, InfiniteStream) ->
    NewElement = Element#xmlel{children = lists:reverse(Element#xmlel.children)},
    NewParent = Parent#xmlel{children = [NewElement | Parent#xmlel.children]},
    parse_events(Rest, [NewParent | Stack], Acc, InfiniteStream);
parse_events([{xml_cdata, _CData} | Rest], [Top], Acc, false) ->
    parse_events(Rest, [Top], Acc, false);
%% This is likely unreachable code.
%% If you need this clause, please provide a test case that covers it.
%% parse_events([{xml_cdata, _CData} | Rest], [], Acc, true) ->
%%     parse_events(Rest, [], Acc, true);
parse_events([{xml_cdata, CData} | Rest],
             [#xmlel{children = [#xmlcdata{content = Content} | RestChildren]} = XML | Stack],
             Acc, InfiniteStream) ->
    NewChildren = [#xmlcdata{content = list_to_binary([Content, CData])} | RestChildren],
    parse_events(Rest, [XML#xmlel{children = NewChildren} | Stack], Acc, InfiniteStream);
parse_events([{xml_cdata, CData} | Rest], [Element | Stack], Acc, InfiniteStream) ->
    NewChildren = [#xmlcdata{content = CData} | Element#xmlel.children],
    parse_events(Rest, [Element#xmlel{children = NewChildren} | Stack], Acc, InfiniteStream).

-spec xml_element(exml:element()) -> exml:element().
xml_element(#xmlel{children = Children} = Element) ->
    Element#xmlel{children = lists:reverse(Children)}.

-spec nss_to_fake_attrs([{binary(), binary() | none}], [exml:attr()]) ->
        [exml:attr()].
nss_to_fake_attrs([{Uri, none} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns">>, Uri} | Acc]);
nss_to_fake_attrs([{Uri, Prefix} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns:", Prefix/binary>>, Uri} | Acc]);
nss_to_fake_attrs([], Acc) ->
    Acc. %% no lists:reverse, as we got the argument list in reversed order


-spec bool_opt(parser_property(), [parser_opt()], boolean()) -> boolean().
bool_opt(Val, Opts, Default) ->
    Got = proplists:get_value(Val, Opts, Default),
    case is_boolean(Got) of
        true -> Got;
        false -> error({invalid_parser_opt, {Val, Got}})
    end.
