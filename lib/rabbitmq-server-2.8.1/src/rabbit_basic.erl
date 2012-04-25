%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2012 VMware, Inc.  All rights reserved.
%%

-module(rabbit_basic).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([publish/4, publish/6, publish/1,
         message/3, message/4, properties/1, append_table_header/3,
         extract_headers/1, replace_headers/2, delivery/4, header_routes/1]).
-export([build_content/2, from_content/1]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(properties_input() ::
        (rabbit_framing:amqp_property_record() | [{atom(), any()}])).
-type(publish_result() ::
        ({ok, rabbit_amqqueue:routing_result(), [pid()]}
         | rabbit_types:error('not_found'))).
-type(headers() :: rabbit_framing:amqp_table() | 'undefined').

-type(exchange_input() :: (rabbit_types:exchange() | rabbit_exchange:name())).
-type(body_input() :: (binary() | [binary()])).

-spec(publish/4 ::
        (exchange_input(), rabbit_router:routing_key(), properties_input(),
         body_input()) -> publish_result()).
-spec(publish/6 ::
        (exchange_input(), rabbit_router:routing_key(), boolean(), boolean(),
         properties_input(), body_input()) -> publish_result()).
-spec(publish/1 ::
        (rabbit_types:delivery()) -> publish_result()).
-spec(delivery/4 ::
        (boolean(), boolean(), rabbit_types:message(), undefined | integer()) ->
                         rabbit_types:delivery()).
-spec(message/4 ::
        (rabbit_exchange:name(), rabbit_router:routing_key(),
         properties_input(), binary()) -> rabbit_types:message()).
-spec(message/3 ::
        (rabbit_exchange:name(), rabbit_router:routing_key(),
         rabbit_types:decoded_content()) ->
                        rabbit_types:ok_or_error2(rabbit_types:message(), any())).
-spec(properties/1 ::
        (properties_input()) -> rabbit_framing:amqp_property_record()).

-spec(append_table_header/3 ::
        (binary(), rabbit_framing:amqp_table(), headers()) -> headers()).

-spec(extract_headers/1 :: (rabbit_types:content()) -> headers()).

-spec(replace_headers/2 :: (headers(), rabbit_types:content())
                           -> rabbit_types:content()).

-spec(header_routes/1 ::
        (undefined | rabbit_framing:amqp_table()) -> [string()]).
-spec(build_content/2 :: (rabbit_framing:amqp_property_record(),
                          binary() | [binary()]) -> rabbit_types:content()).
-spec(from_content/1 :: (rabbit_types:content()) ->
                             {rabbit_framing:amqp_property_record(), binary()}).

-endif.

%%----------------------------------------------------------------------------

%% Convenience function, for avoiding round-trips in calls across the
%% erlang distributed network.
publish(Exchange, RoutingKeyBin, Properties, Body) ->
    publish(Exchange, RoutingKeyBin, false, false, Properties, Body).

%% Convenience function, for avoiding round-trips in calls across the
%% erlang distributed network.
publish(X = #exchange{name = XName}, RKey, Mandatory, Immediate, Props, Body) ->
    publish(X, delivery(Mandatory, Immediate,
                        message(XName, RKey, properties(Props), Body),
                        undefined));
publish(XName, RKey, Mandatory, Immediate, Props, Body) ->
    publish(delivery(Mandatory, Immediate,
                     message(XName, RKey, properties(Props), Body),
                     undefined)).

publish(Delivery = #delivery{
          message = #basic_message{exchange_name = XName}}) ->
    case rabbit_exchange:lookup(XName) of
        {ok, X} -> publish(X, Delivery);
        Err     -> Err
    end.

publish(X, Delivery) ->
    Qs = rabbit_amqqueue:lookup(rabbit_exchange:route(X, Delivery)),
    {RoutingRes, DeliveredQPids} = rabbit_amqqueue:deliver(Qs, Delivery),
    {ok, RoutingRes, DeliveredQPids}.

delivery(Mandatory, Immediate, Message, MsgSeqNo) ->
    #delivery{mandatory = Mandatory, immediate = Immediate, sender = self(),
              message = Message, msg_seq_no = MsgSeqNo}.

build_content(Properties, BodyBin) when is_binary(BodyBin) ->
    build_content(Properties, [BodyBin]);

build_content(Properties, PFR) ->
    %% basic.publish hasn't changed so we can just hard-code amqp_0_9_1
    {ClassId, _MethodId} =
        rabbit_framing_amqp_0_9_1:method_id('basic.publish'),
    #content{class_id = ClassId,
             properties = Properties,
             properties_bin = none,
             protocol = none,
             payload_fragments_rev = PFR}.

from_content(Content) ->
    #content{class_id = ClassId,
             properties = Props,
             payload_fragments_rev = FragmentsRev} =
        rabbit_binary_parser:ensure_content_decoded(Content),
    %% basic.publish hasn't changed so we can just hard-code amqp_0_9_1
    {ClassId, _MethodId} =
        rabbit_framing_amqp_0_9_1:method_id('basic.publish'),
    {Props, list_to_binary(lists:reverse(FragmentsRev))}.

%% This breaks the spec rule forbidding message modification
strip_header(#content{properties = #'P_basic'{headers = undefined}}
             = DecodedContent, _Key) ->
    DecodedContent;
strip_header(#content{properties = Props = #'P_basic'{headers = Headers}}
             = DecodedContent, Key) ->
    case lists:keysearch(Key, 1, Headers) of
        false          -> DecodedContent;
        {value, Found} -> Headers0 = lists:delete(Found, Headers),
                          rabbit_binary_generator:clear_encoded_content(
                            DecodedContent#content{
                              properties = Props#'P_basic'{
                                             headers = Headers0}})
    end.

message(XName, RoutingKey, #content{properties = Props} = DecodedContent) ->
    try
        {ok, #basic_message{
           exchange_name = XName,
           content       = strip_header(DecodedContent, ?DELETED_HEADER),
           id            = rabbit_guid:gen(),
           is_persistent = is_message_persistent(DecodedContent),
           routing_keys  = [RoutingKey |
                            header_routes(Props#'P_basic'.headers)]}}
    catch
        {error, _Reason} = Error -> Error
    end.

message(XName, RoutingKey, RawProperties, Body) ->
    Properties = properties(RawProperties),
    Content = build_content(Properties, Body),
    {ok, Msg} = message(XName, RoutingKey, Content),
    Msg.

properties(P = #'P_basic'{}) ->
    P;
properties(P) when is_list(P) ->
    %% Yes, this is O(length(P) * record_info(size, 'P_basic') / 2),
    %% i.e. slow. Use the definition of 'P_basic' directly if
    %% possible!
    lists:foldl(fun ({Key, Value}, Acc) ->
                        case indexof(record_info(fields, 'P_basic'), Key) of
                            0 -> throw({unknown_basic_property, Key});
                            N -> setelement(N + 1, Acc, Value)
                        end
                end, #'P_basic'{}, P).

append_table_header(Name, Info, undefined) ->
    append_table_header(Name, Info, []);
append_table_header(Name, Info, Headers) ->
    Prior = case rabbit_misc:table_lookup(Headers, Name) of
                undefined          -> [];
                {array, Existing}  -> Existing
            end,
    rabbit_misc:set_table_value(Headers, Name, array, [{table, Info} | Prior]).

extract_headers(Content) ->
    #content{properties = #'P_basic'{headers = Headers}} =
        rabbit_binary_parser:ensure_content_decoded(Content),
    Headers.

replace_headers(Headers, Content = #content{properties = Props}) ->
    rabbit_binary_generator:clear_encoded_content(
      Content#content{properties = Props#'P_basic'{headers = Headers}}).

indexof(L, Element) -> indexof(L, Element, 1).

indexof([], _Element, _N)              -> 0;
indexof([Element | _Rest], Element, N) -> N;
indexof([_ | Rest], Element, N)        -> indexof(Rest, Element, N + 1).

is_message_persistent(#content{properties = #'P_basic'{
                                 delivery_mode = Mode}}) ->
    case Mode of
        1         -> false;
        2         -> true;
        undefined -> false;
        Other     -> throw({error, {delivery_mode_unknown, Other}})
    end.

%% Extract CC routes from headers
header_routes(undefined) ->
    [];
header_routes(HeadersTable) ->
    lists:append(
      [case rabbit_misc:table_lookup(HeadersTable, HeaderKey) of
           {array, Routes} -> [Route || {longstr, Route} <- Routes];
           undefined       -> [];
           {Type, _Val}    -> throw({error, {unacceptable_type_in_header,
                                             Type,
                                             binary_to_list(HeaderKey)}})
       end || HeaderKey <- ?ROUTING_HEADERS]).
