%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(kzd_users).

-export([to_vcard/1]).

-include_lib("whistle/include/wh_types.hrl").

-spec to_vcard(wh_json:object()) -> binary().
to_vcard(JObj) ->
    %% TODO add SOUND, AGENT (X-ASSISTANT), X-MANAGER
    Fields = [
              <<"BEGIN">>
              ,<<"VERSION">>
              ,<<"FN">>
              ,<<"N">>
              ,<<"ORG">>
              ,<<"PHOTO">>
              ,<<"EMAIL">>
              ,<<"BDAY">>
              ,<<"NOTE">>
              ,<<"TITLE">>
              ,<<"ROLE">>
              ,<<"TZ">>
              ,<<"NICKNAME">>
              ,<<"TEL">>
              ,<<"ADR">>
              ,<<"END">>
             ],
    NotEmptyFields = lists:foldl(fun vcard_fields_acc/2, [], [card_field(Key, JObj) || Key <- Fields]),
    PackedFields = lists:reverse([wh_util:join_binary([X, Y], <<":">>) || {X, Y} <- NotEmptyFields]),
    DividedFields = lists:reverse(lists:foldl(fun vcard_field_divide_by_length/2, [], PackedFields)),
    wh_util:join_binary(DividedFields, <<"\n">>).

-spec vcard_escape_chars(binary()) -> binary().
vcard_escape_chars(Val) ->
    Val1 = re:replace(Val, "(:|;|,)", "\\\\&", [global, {return, binary}]),
    re:replace(Val1, "\n", "\\\\n", [global, {return, binary}]).

-spec vcard_fields_acc(vcard_field(), list({ne_binary(), binary()})) -> list({ne_binary(), binary()}).
vcard_fields_acc({_, Val}, Acc) when Val =:= 'undefined'; Val =:= []; Val =:= <<>> ->
    Acc;
vcard_fields_acc({Type, Val}, Acc) ->
    case vcard_normalize_val(Val) of
        <<>> -> Acc;
        ValN ->
            TypeN = vcard_normalize_type(Type),
            [{TypeN, ValN} | Acc]
    end;
vcard_fields_acc([X | Rest], Acc) ->
    vcard_fields_acc(Rest, vcard_fields_acc(X, Acc));
vcard_fields_acc([], Acc) ->
    Acc.

-spec vcard_normalize_val(binary() | {char(), binaries()}) -> binary().
vcard_normalize_val({Separator, Vals}) when is_list(Vals) ->
    wh_util:join_binary([vcard_escape_chars(X) || X <- Vals, X =/= 'undefined', X =/= [], X =/= <<>>], Separator);
vcard_normalize_val(Val) when is_binary(Val) ->
    vcard_escape_chars(Val).

-spec vcard_normalize_type(list() | {ne_binary(), ne_binary()} | ne_binary()) -> ne_binary().
vcard_normalize_type(T) when is_list(T) -> wh_util:join_binary([vcard_normalize_type(X) || X <- T], <<";">>);
vcard_normalize_type({T, V}) -> wh_util:join_binary([T, V], <<"=">>);
vcard_normalize_type(T) -> T.

-type vcard_val() :: binary() | {char(), binaries()} | 'undefined'.
-type vcard_type_token() :: ne_binary() | {ne_binary(), ne_binary()}.
-type vcard_type() :: list(vcard_type_token()) | vcard_type_token().
-type vcard_field_token() :: {vcard_type(), vcard_val()}.
-type vcard_field() :: vcard_field_token() | list(vcard_field_token()).

-spec card_field(ne_binary(), wh_json:object()) -> vcard_field().
card_field(Key = <<"BEGIN">>, _) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"VERSION">>, _) ->
    {Key, <<"3.0">>};
card_field(Key = <<"END">>, _) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"FN">>, JObj) ->
    FirstName = wh_json:get_value(<<"first_name">>, JObj),
    LastName = wh_json:get_value(<<"last_name">>, JObj),
    MiddleName = wh_json:get_value(<<"middle_name">>, JObj),
    {Key, wh_util:join_binary([X || X <- [FirstName, MiddleName, LastName]
                                    ,X =/= undefined
                                    ,X =/= []
                                    ,X =/= <<>>]
                              ,<<" ">>)};
card_field(Key = <<"N">>, JObj) ->
    FirstName = wh_json:get_value(<<"first_name">>, JObj),
    LastName = wh_json:get_value(<<"last_name">>, JObj),
    MiddleName = wh_json:get_value(<<"middle_name">>, JObj),
    {Key, {$;, [LastName, FirstName, MiddleName]}};
card_field(Key = <<"ORG">>, JObj) ->
    {Key, wh_json:get_value(<<"org">>, JObj)};
card_field(Key = <<"PHOTO">>, JObj) ->
    case wh_json:get_value(<<"photo">>, JObj) of
        'undefined' -> {Key, 'undefined'};
        PhotoJObj ->
            [{CT, PhotoBin}] = wh_json:to_proplist(PhotoJObj),
            TypeType = case CT of
                           <<"image/jpeg">> -> <<"JPEG">>
                       end,
            Data = base64:encode(PhotoBin),
            {[Key, {<<"ENCODING">>, <<"B">>}, {<<"TYPE">>, TypeType}], Data}
    end;
card_field(Key = <<"ADR">>, JObj) ->
    Addresses = lists:map(fun normalize_address/1, wh_json:get_value(<<"addresses">>, JObj, [])),
    [{[Key, {<<"TYPE">>, Type}], Address} || {Type, Address} <- Addresses];
card_field(Key = <<"TEL">>, JObj) ->
    CallerId = wh_json:get_value(<<"caller_id">>, JObj, wh_json:new()),
    Internal = wh_json:get_value(<<"internal">>, CallerId),
    External = wh_json:get_value(<<"external">>, CallerId),
    [
     {Key, Internal}
     ,{Key, External}
    ];
card_field(Key = <<"EMAIL">>, JObj) ->
    {Key, wh_json:get_value(<<"email">>, JObj)};
card_field(Key = <<"BDAY">>, JObj) ->
    {Key, wh_json:get_value(<<"birthday">>, JObj)};
card_field(Key = <<"NOTE">>, JObj) ->
    {Key, wh_json:get_value(<<"note">>, JObj)};
card_field(Key = <<"TITLE">>, JObj) ->
    {Key, wh_json:get_value(<<"title">>, JObj)};
card_field(Key = <<"ROLE">>, JObj) ->
    {Key, wh_json:get_value(<<"role">>, JObj)};
card_field(Key = <<"TZ">>, JObj) ->
    {Key, wh_json:get_value(<<"timezone">>, JObj)};
card_field(Key = <<"NICKNAME">>, JObj) ->
    {Key, {$,, wh_json:get_value(<<"nicknames">>, JObj, [])}}.

-spec vcard_field_divide_by_length(binary(), binaries()) -> binaries().
vcard_field_divide_by_length(<<Row:75/binary, Rest/binary>>, Acc) ->
    vcard_field_divide_by_length(Rest, [Row | Acc]);
vcard_field_divide_by_length(Row, Acc) ->
    [Row | Acc].

-spec normalize_address(wh_json:object()) -> {ne_binary(), binary()}.
normalize_address(JObj) ->
    Types = case wh_json:get_value(<<"types">>, JObj) of
                'undefined' -> [<<"intl">>, <<"postal">>, <<"parcel">>, <<"work">>];
                T -> T
            end,
    Address = wh_json:get_value(<<"address">>, JObj),
    {wh_util:join_binary(Types, <<",">>), Address}.
