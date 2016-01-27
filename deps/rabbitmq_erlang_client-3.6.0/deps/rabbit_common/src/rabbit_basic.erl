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
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_basic).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([publish/4, publish/5, publish/1,
         message/3, message/4, properties/1, prepend_table_header/3,
         extract_headers/1, extract_timestamp/1, map_headers/2, delivery/4,
         header_routes/1, parse_expiration/1, header/2, header/3]).
-export([build_content/2, from_content/1, msg_size/1, maybe_gc_large_msg/1]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(properties_input() ::
        (rabbit_framing:amqp_property_record() | [{atom(), any()}])).
-type(publish_result() ::
        ({ok, [pid()]} | rabbit_types:error('not_found'))).
-type(header() :: any()).
-type(headers() :: rabbit_framing:amqp_table() | 'undefined').

-type(exchange_input() :: (rabbit_types:exchange() | rabbit_exchange:name())).
-type(body_input() :: (binary() | [binary()])).

-spec(publish/4 ::
        (exchange_input(), rabbit_router:routing_key(), properties_input(),
         body_input()) -> publish_result()).
-spec(publish/5 ::
        (exchange_input(), rabbit_router:routing_key(), boolean(),
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

-spec(prepend_table_header/3 ::
        (binary(), rabbit_framing:amqp_table(), headers()) -> headers()).

-spec(header/2 ::
        (header(), headers()) -> 'undefined' | any()).
-spec(header/3 ::
        (header(), headers(), any()) -> 'undefined' | any()).

-spec(extract_headers/1 :: (rabbit_types:content()) -> headers()).

-spec(map_headers/2 :: (fun((headers()) -> headers()), rabbit_types:content())
                       -> rabbit_types:content()).

-spec(header_routes/1 ::
        (undefined | rabbit_framing:amqp_table()) -> [string()]).
-spec(build_content/2 :: (rabbit_framing:amqp_property_record(),
                          binary() | [binary()]) -> rabbit_types:content()).
-spec(from_content/1 :: (rabbit_types:content()) ->
                             {rabbit_framing:amqp_property_record(), binary()}).
-spec(parse_expiration/1 ::
        (rabbit_framing:amqp_property_record())
        -> rabbit_types:ok_or_error2('undefined' | non_neg_integer(), any())).

-spec(msg_size/1 :: (rabbit_types:content() | rabbit_types:message()) ->
                         non_neg_integer()).

-spec(maybe_gc_large_msg/1 ::
        (rabbit_types:content() | rabbit_types:message()) -> non_neg_integer()).

-endif.

%%----------------------------------------------------------------------------

%% Convenience function, for avoiding round-trips in calls across the
%% erlang distributed network.
publish(Exchange, RoutingKeyBin, Properties, Body) ->
    publish(Exchange, RoutingKeyBin, false, Properties, Body).

%% Convenience function, for avoiding round-trips in calls across the
%% erlang distributed network.
publish(X = #exchange{name = XName}, RKey, Mandatory, Props, Body) ->
    Message = message(XName, RKey, properties(Props), Body),
    publish(X, delivery(Mandatory, false, Message, undefined));
publish(XName, RKey, Mandatory, Props, Body) ->
    Message = message(XName, RKey, properties(Props), Body),
    publish(delivery(Mandatory, false, Message, undefined)).

publish(Delivery = #delivery{
          message = #basic_message{exchange_name = XName}}) ->
    case rabbit_exchange:lookup(XName) of
        {ok, X} -> publish(X, Delivery);
        Err     -> Err
    end.

publish(X, Delivery) ->
    Qs = rabbit_amqqueue:lookup(rabbit_exchange:route(X, Delivery)),
    DeliveredQPids = rabbit_amqqueue:deliver(Qs, Delivery),
    {ok, DeliveredQPids}.

delivery(Mandatory, Confirm, Message, MsgSeqNo) ->
    #delivery{mandatory = Mandatory, confirm = Confirm, sender = self(),
              message = Message, msg_seq_no = MsgSeqNo, flow = noflow}.

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

prepend_table_header(Name, Info, undefined) ->
    prepend_table_header(Name, Info, []);
prepend_table_header(Name, Info, Headers) ->
    case rabbit_misc:table_lookup(Headers, Name) of
        {array, Existing} ->
            prepend_table(Name, Info, Existing, Headers);
        undefined ->
            prepend_table(Name, Info, [], Headers);
        Other ->
            Headers2 = prepend_table(Name, Info, [], Headers),
            set_invalid_header(Name, Other, Headers2)
    end.

prepend_table(Name, Info, Prior, Headers) ->
    rabbit_misc:set_table_value(Headers, Name, array, [{table, Info} | Prior]).

set_invalid_header(Name, {_, _}=Value, Headers) when is_list(Headers) ->
    case rabbit_misc:table_lookup(Headers, ?INVALID_HEADERS_KEY) of
        undefined ->
            set_invalid([{Name, array, [Value]}], Headers);
        {table, ExistingHdr} ->
            update_invalid(Name, Value, ExistingHdr, Headers);
        Other ->
            %% somehow the x-invalid-headers header is corrupt
            Invalid = [{?INVALID_HEADERS_KEY, array, [Other]}],
            set_invalid_header(Name, Value, set_invalid(Invalid, Headers))
    end.

set_invalid(NewHdr, Headers) ->
    rabbit_misc:set_table_value(Headers, ?INVALID_HEADERS_KEY, table, NewHdr).

update_invalid(Name, Value, ExistingHdr, Header) ->
    Values = case rabbit_misc:table_lookup(ExistingHdr, Name) of
                 undefined      -> [Value];
                 {array, Prior} -> [Value | Prior]
             end,
    NewHdr = rabbit_misc:set_table_value(ExistingHdr, Name, array, Values),
    set_invalid(NewHdr, Header).

header(_Header, undefined) ->
    undefined;
header(_Header, []) ->
    undefined;
header(Header, Headers) ->
    header(Header, Headers, undefined).

header(Header, Headers, Default) ->
    case lists:keysearch(Header, 1, Headers) of
        false        -> Default;
        {value, Val} -> Val
    end.

extract_headers(Content) ->
    #content{properties = #'P_basic'{headers = Headers}} =
        rabbit_binary_parser:ensure_content_decoded(Content),
    Headers.

extract_timestamp(Content) ->
    #content{properties = #'P_basic'{timestamp = Timestamp}} =
        rabbit_binary_parser:ensure_content_decoded(Content),
    Timestamp.

map_headers(F, Content) ->
    Content1 = rabbit_binary_parser:ensure_content_decoded(Content),
    #content{properties = #'P_basic'{headers = Headers} = Props} = Content1,
    Headers1 = F(Headers),
    rabbit_binary_generator:clear_encoded_content(
      Content1#content{properties = Props#'P_basic'{headers = Headers1}}).

indexof(L, Element) -> indexof(L, Element, 1).

indexof([],               _Element, _N) -> 0;
indexof([Element | _Rest], Element,  N) -> N;
indexof([_ | Rest],        Element,  N) -> indexof(Rest, Element, N + 1).

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
                                             binary_to_list(HeaderKey), Type}})
       end || HeaderKey <- ?ROUTING_HEADERS]).

parse_expiration(#'P_basic'{expiration = undefined}) ->
    {ok, undefined};
parse_expiration(#'P_basic'{expiration = Expiration}) ->
    case string:to_integer(binary_to_list(Expiration)) of
        {error, no_integer} = E ->
            E;
        {N, ""} ->
            case rabbit_misc:check_expiry(N) of
                ok             -> {ok, N};
                E = {error, _} -> E
            end;
        {_, S} ->
            {error, {leftover_string, S}}
    end.

%% Some processes (channel, writer) can get huge amounts of binary
%% garbage when processing huge messages at high speed (since we only
%% do enough reductions to GC every few hundred messages, and if each
%% message is 1MB then that's ugly). So count how many bytes of
%% message we have processed, and force a GC every so often.
maybe_gc_large_msg(Content) ->
    Size = msg_size(Content),
    Current = case get(msg_size_for_gc) of
                  undefined -> 0;
                  C         -> C
              end,
    New = Current + Size,
    put(msg_size_for_gc, case New > 1000000 of
                             true  -> erlang:garbage_collect(),
                                      0;
                             false -> New
                         end),
    Size.

msg_size(#content{payload_fragments_rev = PFR}) -> iolist_size(PFR);
msg_size(#basic_message{content = Content})     -> msg_size(Content).
