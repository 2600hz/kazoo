%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Diversion SIP header manipulation.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzsip_diversion).

-export([to_binary/1
        ,from_binary/1
        ]).

-export([reason/1, set_reason/2
        ,counter/1, set_counter/2
        ,limit/1
        ,privacy/1
        ,screen/1
        ,extensions/1
        ,address/1, set_address/2
        ,user/1, set_user/2
        ,new/0
        ]).

-ifdef(TEST).
-export([parse_name_addr_header/1]).
-endif.

-include("kazoo_sip.hrl").

-define(PARAM_REASON, <<"reason">>).
-define(PARAM_COUNTER, <<"counter">>).
-define(PARAM_LIMIT, <<"limit">>).
-define(PARAM_PRIVACY, <<"privacy">>).
-define(PARAM_SCREEN, <<"screen">>).
-define(PARAM_EXTENSION, <<"extension">>).
-define(PARAM_ADDRESS, <<"address">>).

-define(SOLO_EXTENSION, <<"_solo_">>).

-type diversion() :: kz_json:object().
-export_type([diversion/0]).

-spec new() -> diversion().
new() -> kz_json:new().


-spec user(diversion()) -> kz_term:api_binary().
user(JObj) ->
    kzsip_uri:user(kzsip_uri:parse(address(JObj))).

-spec address(diversion()) -> kz_term:api_binary().
address(JObj) ->
    kz_json:get_ne_binary_value(?PARAM_ADDRESS, JObj).

-spec reason(diversion()) -> kz_term:api_binary().
reason(JObj) ->
    kz_json:get_ne_binary_value(?PARAM_REASON, JObj).

-spec counter(diversion()) -> non_neg_integer().
counter(JObj) ->
    kz_json:get_integer_value(?PARAM_COUNTER, JObj, 0).

-spec limit(diversion()) -> kz_term:api_integer().
limit(JObj) ->
    kz_json:get_integer_value(?PARAM_LIMIT, JObj).

-spec privacy(diversion()) -> kz_term:api_binary().
privacy(JObj) ->
    kz_json:get_ne_binary_value(?PARAM_PRIVACY, JObj).

-spec screen(diversion()) -> kz_term:api_binary().
screen(JObj) ->
    kz_json:get_ne_binary_value(?PARAM_SCREEN, JObj).

-spec extensions(diversion()) -> kz_term:api_list().
extensions(JObj) ->
    case kz_json:get_ne_value(?PARAM_EXTENSION, JObj) of
        'undefined' -> 'undefined';
        Extensions ->
            lists:foldl(fun extensions_fold/2, [], kz_json:to_proplist(Extensions))
    end.

-spec extensions_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) ->
                             kz_term:proplist().
extensions_fold({K, ?SOLO_EXTENSION}, Acc) ->
    [K | Acc];
extensions_fold({_K, _V}=Extention, Acc) ->
    [Extention | Acc].


-spec set_user(diversion(), kz_term:ne_binary()) -> diversion().
set_user(JObj, User) ->
    Address = kzsip_uri:parse(address(JObj)),
    Address1 = kzsip_uri:set_user(Address, User),
    set_address(JObj, list_to_binary([<<"<">>, kzsip_uri:encode(Address1), <<">">>])).

-spec set_address(diversion(), kz_term:ne_binary()) -> diversion().
set_address(JObj, Address) ->
    kz_json:set_value(?PARAM_ADDRESS, Address, JObj).

-spec set_reason(diversion(), kz_term:ne_binary()) -> diversion().
set_reason(JObj, Reason) ->
    kz_json:set_value(?PARAM_REASON, Reason, JObj).

-spec set_counter(diversion(), non_neg_integer()) -> diversion().
set_counter(JObj, Counter) ->
    kz_json:set_value(?PARAM_COUNTER, Counter, JObj).

-spec from_binary(kz_term:ne_binary()) -> kz_json:object().
from_binary(<<"Diversion:", Header/binary>>) ->
    from_binary(Header);
from_binary(Header) ->
    case parse_name_addr_header(Header) of
        {<<>>, Name} ->
            kz_json:from_list([{?PARAM_ADDRESS, Name}]);
        {Rest, Name} ->
            parse_params(Rest, kz_json:set_value(?PARAM_ADDRESS, Name, kz_json:new()))
    end.

-spec parse_name_addr_header(kz_term:ne_binary()) -> {binary(), kz_term:ne_binary()}.
parse_name_addr_header(Header) ->
    Start = lists:foldl(fun name_addr_start/2, Header, [$\n, $\s]),
    parse_name_addr({Start, []}).

-spec parse_name_addr({binary(), list()}) -> {binary(), kz_term:ne_binary()}.
parse_name_addr({<<"<", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_angle(Header, [$< | Acc]));
parse_name_addr({<<"\"", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_double_quote(Header, [$" | Acc]));
parse_name_addr({<<"'", Header/binary>>, Acc}) ->
    parse_name_addr(parse_name_addr_single_quote(Header, [$" | Acc]));
parse_name_addr({<<"\n", Header/binary>>, Acc}) ->
    {Header, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<";", Header/binary>>, Acc}) ->
    {Header, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<>>, Acc}) ->
    {<<>>, list_to_binary(lists:reverse(Acc))};
parse_name_addr({<<C, Header/binary>>, Acc}) ->
    parse_name_addr({Header, [C | Acc]}).

-spec parse_name_addr_angle(kz_term:ne_binary(), iolist()) ->
                                   {binary(), iolist()}.
parse_name_addr_angle(<<">", Header/binary>>, Acc) ->
    {kz_binary:strip(Header), [$> | Acc]};
parse_name_addr_angle(<<C, Header/binary>>, Acc) ->
    parse_name_addr_angle(Header, [C | Acc]).

-spec parse_name_addr_double_quote(kz_term:ne_binary(), iolist()) ->
                                          {binary(), iolist()}.
parse_name_addr_double_quote(<<"\"", Header/binary>>, Acc) ->
    {Header, [$" | Acc]};
parse_name_addr_double_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_double_quote(Header, [C | Acc]).

-spec parse_name_addr_single_quote(kz_term:ne_binary(), iolist()) ->
                                          {binary(), iolist()}.
parse_name_addr_single_quote(<<"'", Header/binary>>, Acc) ->
    {Header, [$' | Acc]};
parse_name_addr_single_quote(<<C, Header/binary>>, Acc) ->
    parse_name_addr_single_quote(Header, [C | Acc]).

-spec name_addr_start(char(), kz_term:ne_binary()) -> kz_term:ne_binary().
name_addr_start(Char, Bin) ->
    kz_binary:strip_left(Bin, Char).

-spec parse_params(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
parse_params(Params, JObj) ->
    case binary:split(Params, <<";">>, ['trim']) of
        [Param] -> parse_param(Param, JObj);
        [Param, Rest] -> parse_params(Rest, parse_param(Param, JObj))
    end.

-spec parse_param(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
parse_param(Param, JObj) ->
    case binary:split(Param, <<"=">>, ['trim']) of
        [Name, Value] ->
            parse_param(kz_binary:strip(Name), kz_binary:strip(Value), JObj);
        [Extension] ->
            add_extension(kz_binary:strip(Extension), JObj)
    end.

-spec parse_param(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
parse_param(?PARAM_REASON, Value, JObj) ->
    kz_json:set_value(?PARAM_REASON, parse_reason_param(Value), JObj);
parse_param(?PARAM_COUNTER, Value, JObj) ->
    kz_json:set_value(?PARAM_COUNTER, kz_term:to_integer(Value), JObj);
parse_param(?PARAM_LIMIT, Value, JObj) ->
    kz_json:set_value(?PARAM_LIMIT, kz_term:to_integer(Value), JObj);
parse_param(?PARAM_PRIVACY, Value, JObj) ->
    kz_json:set_value(?PARAM_PRIVACY, parse_privacy_param(Value), JObj);
parse_param(?PARAM_SCREEN, Value, JObj) ->
    kz_json:set_value(?PARAM_SCREEN, parse_screen_param(Value), JObj);
parse_param(Extension, Value, JObj) ->
    add_extension(Extension, maybe_unquote(Value), JObj).

-spec add_extension(binary(), kz_json:object()) ->
                           kz_json:object().
add_extension(<<>>, JObj) -> JObj;
add_extension(Extension, JObj) ->
    Extensions = kz_json:get_value(?PARAM_EXTENSION, JObj, kz_json:new()),
    kz_json:set_value(?PARAM_EXTENSION
                     ,kz_json:set_value(maybe_unquote(Extension)
                                       ,?SOLO_EXTENSION
                                       ,Extensions
                                       )
                     ,JObj
                     ).

-spec add_extension(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                           kz_json:object().
add_extension(Extension, Value, JObj) ->
    Extensions = kz_json:get_value(?PARAM_EXTENSION, JObj, kz_json:new()),
    kz_json:set_value(?PARAM_EXTENSION
                     ,kz_json:set_value(Extension
                                       ,maybe_unquote(Value)
                                       ,Extensions
                                       )
                     ,JObj
                     ).

-define(PARAM_REASON_LITERALS, [<<"unknown">>
                               ,<<"user-busy">>
                               ,<<"no-answer">>
                               ,<<"unavailable">>
                               ,<<"unconditional">>
                               ,<<"time-of-day">>
                               ,<<"do-not-disturb">>
                               ,<<"deflection">>
                               ,<<"follow-me">>
                               ,<<"out-of-service">>
                               ,<<"away">>
                               ]).
parse_reason_param(Reason) ->
    parse_param_value(Reason, ?PARAM_REASON_LITERALS).

-define(PARAM_PRIVACY_LITERALS, [<<"full">>
                                ,<<"name">>
                                ,<<"uri">>
                                ,<<"off">>
                                ]).
parse_privacy_param(Privacy) ->
    parse_param_value(Privacy, ?PARAM_PRIVACY_LITERALS).

-define(PARAM_SCREEN_LITERALS, [<<"yes">>, <<"no">>]).
parse_screen_param(Screen) ->
    parse_param_value(Screen, ?PARAM_SCREEN_LITERALS).

-spec parse_param_value(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binary().
parse_param_value(<<"\"", _/binary>> = Param, _Literals) ->
    parse_quoted_param(Param);
parse_param_value(<<"'", _/binary>> = Param, _Literals) ->
    parse_quoted_param(Param);
parse_param_value(Param, [Literal | Literals]) ->
    LiteralSize = byte_size(Literal),
    case binary:longest_common_prefix([Param, Literal]) of
        LiteralSize -> Literal;
        _ -> parse_param_value(Param, Literals)
    end;
parse_param_value(Param, []) ->
    parse_token_param(Param).

-spec parse_token_param(kz_term:ne_binary()) -> binary().
parse_token_param(Token) ->
    parse_token_param(Token, []).

-spec parse_token_param(kz_term:ne_binary(), iolist()) -> binary().
parse_token_param(<<$\s, _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<$\n, _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_token_param(<<C, Token/binary>>, Acc) ->
    parse_token_param(Token, [C | Acc]).

-spec parse_quoted_param(kz_term:ne_binary()) -> kz_term:ne_binary().
parse_quoted_param(<<"\"", Quoted/binary>>) ->
    parse_double_quoted_param(Quoted, []);
parse_quoted_param(<<"'", Quoted/binary>>) ->
    parse_single_quoted_param(Quoted, []).

-spec parse_double_quoted_param(kz_term:ne_binary(), iolist()) -> kz_term:ne_binary().
parse_double_quoted_param(<<"\"", _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_double_quoted_param(<<C, Quoted/binary>>, Acc) ->
    parse_double_quoted_param(Quoted, [C | Acc]).

-spec parse_single_quoted_param(kz_term:ne_binary(), iolist()) -> kz_term:ne_binary().
parse_single_quoted_param(<<"'", _/binary>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
parse_single_quoted_param(<<C, Quoted/binary>>, Acc) ->
    parse_single_quoted_param(Quoted, [C | Acc]).

-spec maybe_unquote(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_unquote(<<"\"", Value/binary>>) ->
    kz_binary:truncate_right(Value, byte_size(Value)-1);
maybe_unquote(Value) -> Value.

-spec to_binary(kz_json:object()) -> kz_term:ne_binary().
to_binary(JObj) ->
    Address = kz_json:get_value(?PARAM_ADDRESS, JObj),
    maybe_add_params(JObj, fix_address(Address)).

-spec fix_address(kz_term:ne_binary()) -> kz_term:ne_binary().
fix_address(<<$", _Rest/binary>> = Address) -> Address;
fix_address(<<"<sip", _Rest/binary>> = Address) -> Address;
fix_address(<<"sip", _Rest/binary>> = Address) ->
    list_to_binary([$<, Address, $>]);
fix_address(Address) -> Address.

-spec maybe_add_params(kz_json:object(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_params(JObj, Address) ->
    kz_json:foldl(fun maybe_add_param/3, Address, JObj).

-spec maybe_add_param(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_param(?PARAM_REASON, Reason, Acc) ->
    <<Acc/binary, ";", ?PARAM_REASON/binary, "=", (encode_reason_param(kz_binary:strip(Reason)))/binary>>;
maybe_add_param(?PARAM_COUNTER, Count, Acc) ->
    <<Acc/binary, ";", ?PARAM_COUNTER/binary, "=", (kz_term:to_binary(Count))/binary>>;
maybe_add_param(?PARAM_LIMIT, Limit, Acc) ->
    <<Acc/binary, ";", ?PARAM_LIMIT/binary, "=", (kz_term:to_binary(Limit))/binary>>;
maybe_add_param(?PARAM_PRIVACY, Priv, Acc) ->
    <<Acc/binary, ";", ?PARAM_PRIVACY/binary, "=", (encode_privacy_param(kz_binary:strip(Priv)))/binary>>;
maybe_add_param(?PARAM_SCREEN, Screen, Acc) ->
    <<Acc/binary, ";", ?PARAM_SCREEN/binary, "=", (encode_screen_param(kz_binary:strip(Screen)))/binary>>;
maybe_add_param(?PARAM_EXTENSION, Extensions, Acc) ->
    lists:foldl(fun maybe_add_extension/2, Acc, kz_json:to_proplist(Extensions));
maybe_add_param(_, _, Acc) -> Acc.

-spec maybe_add_extension({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_extension({Key, ?SOLO_EXTENSION}, Acc) ->
    <<Acc/binary, ";", (kz_term:to_binary(Key))/binary>>;
maybe_add_extension({Key, Value}, Acc) ->
    <<Acc/binary, ";", (kz_term:to_binary(Key))/binary, "=", (kz_term:to_binary(Value))/binary>>;
maybe_add_extension(Key, Acc) ->
    <<Acc/binary, ";", (kz_term:to_binary(Key))/binary>>.

-spec encode_reason_param(kz_term:ne_binary()) -> kz_term:ne_binary().
encode_reason_param(Reason) ->
    encode_param(Reason, ?PARAM_REASON_LITERALS).
encode_privacy_param(Privacy) ->
    encode_param(Privacy, ?PARAM_PRIVACY_LITERALS).
encode_screen_param(Screen) ->
    encode_param(Screen, ?PARAM_SCREEN_LITERALS).

-spec encode_param(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binary().
encode_param(Param, Literals) ->
    case lists:member(Param, Literals) of
        'true' -> Param;
        'false' -> quote_param(Param)
    end.

-spec quote_param(kz_term:ne_binary()) -> kz_term:ne_binary().
quote_param(Param) ->
    <<"\"", Param/binary, "\"">>.
