%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_user).

-export([email/1, email/2
         ,voicemail_notification_enabled/1, voicemail_notification_enabled/2
         ,to_vcard/1
         ,timezone/1, timezone/2
         ,presence_id/1, presence_id/2, set_presence_id/2
         ,is_enabled/1, is_enabled/2
         ,enable/1, disable/1
         ,type/0
         ,devices/1
         ,fetch/2
         ,fax_settings/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_EMAIL, <<"email">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_PRESENCE_ID, <<"presence_id">>).
-define(KEY_IS_ENABLED, <<"enabled">>).

-define(PVT_TYPE, <<"user">>).

-spec email(doc()) -> api_binary().
-spec email(doc(), Default) -> ne_binary() | Default.
email(User) ->
    email(User, 'undefined').
email(User, Default) ->
    kz_json:get_value(?KEY_EMAIL, User, Default).

-spec voicemail_notification_enabled(doc()) -> boolean().
-spec voicemail_notification_enabled(doc(), Default) -> boolean() | Default.
voicemail_notification_enabled(User) ->
    voicemail_notification_enabled(User, 'false').
voicemail_notification_enabled(User, Default) ->
    kz_json:is_true(<<"vm_to_email_enabled">>, User, Default).

-spec to_vcard(kz_json:object()) -> binary().
to_vcard(JObj) ->
    %% TODO add SOUND, AGENT (X-ASSISTANT), X-MANAGER
    Fields = [<<"BEGIN">>
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
    NotEmptyFields = lists:foldl(fun vcard_fields_acc/2
                                 ,[]
                                 ,[card_field(Key, JObj) || Key <- Fields]
                                ),
    PackedFields = lists:reverse(
                     [kz_util:join_binary([X, Y], <<":">>) ||
                         {X, Y} <- NotEmptyFields
                     ]
                    ),
    DividedFields = lists:reverse(
                      lists:foldl(fun vcard_field_divide_by_length/2, [], PackedFields)
                     ),
    kz_util:join_binary(DividedFields, <<"\n">>).

-spec vcard_escape_chars(binary()) -> binary().
vcard_escape_chars(Val) ->
    Val1 = re:replace(Val, "(:|;|,)", "\\\\&", ['global', {'return', 'binary'}]),
    re:replace(Val1, "\n", "\\\\n", ['global', {'return', 'binary'}]).

-spec vcard_fields_acc(vcard_field(), [{ne_binary(), binary()}]) -> [{ne_binary(), binary()}].
vcard_fields_acc({_, Val}, Acc)
  when Val =:= 'undefined'; Val =:= []; Val =:= <<>> ->
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
    kz_util:join_binary([vcard_escape_chars(X) || X <- Vals, not kz_util:is_empty(X)], Separator);
vcard_normalize_val(Val) when is_binary(Val) ->
    vcard_escape_chars(Val).

-spec vcard_normalize_type(list() | {ne_binary(), ne_binary()} | ne_binary()) -> ne_binary().
vcard_normalize_type(T) when is_list(T) -> kz_util:join_binary([vcard_normalize_type(X) || X <- T], <<";">>);
vcard_normalize_type({T, V}) -> kz_util:join_binary([T, V], <<"=">>);
vcard_normalize_type(T) -> T.

-type vcard_val() :: binary() | {char(), binaries()} | 'undefined'.
-type vcard_type_token() :: ne_binary() | {ne_binary(), ne_binary()}.
-type vcard_type() :: [vcard_type_token()] | vcard_type_token().
-type vcard_field_token() :: {vcard_type(), vcard_val()}.
-type vcard_field() :: vcard_field_token() | [vcard_field_token()].

-spec card_field(ne_binary(), kz_json:object()) -> vcard_field().
card_field(Key = <<"BEGIN">>, _) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"VERSION">>, _) ->
    {Key, <<"3.0">>};
card_field(Key = <<"END">>, _) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"FN">>, JObj) ->
    FirstName = kz_json:get_value(<<"first_name">>, JObj),
    LastName = kz_json:get_value(<<"last_name">>, JObj),
    MiddleName = kz_json:get_value(<<"middle_name">>, JObj),
    {Key
     ,kz_util:join_binary([X || X <- [FirstName, MiddleName, LastName],
                                not kz_util:is_empty(X)
                          ]
                          ,<<" ">>
                         )
    };
card_field(Key = <<"N">>, JObj) ->
    FirstName = kz_json:get_value(<<"first_name">>, JObj),
    LastName = kz_json:get_value(<<"last_name">>, JObj),
    MiddleName = kz_json:get_value(<<"middle_name">>, JObj),
    {Key, {$;, [LastName, FirstName, MiddleName]}};
card_field(Key = <<"ORG">>, JObj) ->
    {Key, kz_json:get_value(<<"org">>, JObj)};
card_field(Key = <<"PHOTO">>, JObj) ->
    case kz_json:get_value(<<"photo">>, JObj) of
        'undefined' -> {Key, 'undefined'};
        PhotoJObj ->
            [{CT, PhotoBin}] = kz_json:to_proplist(PhotoJObj),
            TypeType = content_type_to_type(CT),
            Data = base64:encode(PhotoBin),
            {[Key, {<<"ENCODING">>, <<"B">>}, {<<"TYPE">>, TypeType}], Data}
    end;
card_field(Key = <<"ADR">>, JObj) ->
    Addresses = lists:map(fun normalize_address/1, kz_json:get_value(<<"addresses">>, JObj, [])),
    [{[Key, {<<"TYPE">>, Type}], Address} || {Type, Address} <- Addresses];
card_field(Key = <<"TEL">>, JObj) ->
    CallerId = kz_json:get_value(<<"caller_id">>, JObj, kz_json:new()),
    Internal = kz_json:get_value(<<"internal">>, CallerId),
    External = kz_json:get_value(<<"external">>, CallerId),

    [{Key, Internal}
     ,{Key, External}
    ];
card_field(Key = <<"EMAIL">>, JObj) ->
    {Key, kz_json:get_value(<<"email">>, JObj)};
card_field(Key = <<"BDAY">>, JObj) ->
    {Key, kz_json:get_value(<<"birthday">>, JObj)};
card_field(Key = <<"NOTE">>, JObj) ->
    {Key, kz_json:get_value(<<"note">>, JObj)};
card_field(Key = <<"TITLE">>, JObj) ->
    {Key, kz_json:get_value(<<"title">>, JObj)};
card_field(Key = <<"ROLE">>, JObj) ->
    {Key, kz_json:get_value(<<"role">>, JObj)};
card_field(Key = <<"TZ">>, JObj) ->
    {Key, kz_json:get_value(?KEY_TIMEZONE, JObj)};
card_field(Key = <<"NICKNAME">>, JObj) ->
    {Key, {$,, kz_json:get_value(<<"nicknames">>, JObj, [])}}.

-spec content_type_to_type(ne_binary()) -> ne_binary().
content_type_to_type(<<"image/jpeg">>) -> <<"JPEG">>.

-spec vcard_field_divide_by_length(binary(), binaries()) -> binaries().
vcard_field_divide_by_length(<<Row:75/binary>>, Acc) ->
    [Row | Acc];
vcard_field_divide_by_length(<<Row:75/binary, Rest/binary>>, Acc) ->
    vcard_field_divide_by_length(Rest, [Row | Acc]);
vcard_field_divide_by_length(Row, Acc) ->
    [Row | Acc].

-spec normalize_address(kz_json:object()) -> {ne_binary(), binary()}.
normalize_address(JObj) ->
    Types = case kz_json:get_value(<<"types">>, JObj) of
                'undefined' -> [<<"intl">>, <<"postal">>, <<"parcel">>, <<"work">>];
                T -> T
            end,
    Address = kz_json:get_value(<<"address">>, JObj),
    {kz_util:join_binary(Types, <<",">>), Address}.

-spec timezone(kz_json:object()) -> api_binary().
-spec timezone(kz_json:object(), Default) -> ne_binary() | Default.
timezone(JObj) ->
    timezone(JObj, 'undefined').
timezone(JObj, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, JObj, Default) of
        <<"inherit">> -> kz_account:timezone(kz_doc:account_id(JObj));  %% UI-1808
        'undefined' -> kz_account:timezone(kz_doc:account_id(JObj));
        TZ -> TZ
    end.

-spec presence_id(doc()) -> api_binary().
-spec presence_id(doc(), Default) -> ne_binary() | Default.
presence_id(UserJObj) ->
    presence_id(UserJObj, 'undefined').
presence_id(UserJObj, Default) ->
    kz_json:get_binary_value(?KEY_PRESENCE_ID, UserJObj, Default).

-spec set_presence_id(doc(), ne_binary()) -> doc().
set_presence_id(UserJObj, Id) ->
    kz_json:set_value(
      ?KEY_PRESENCE_ID
      ,kz_util:to_binary(Id)
      ,UserJObj
     ).

-spec is_enabled(doc()) -> boolean().
-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(JObj) ->
    is_enabled(JObj, 'true').
is_enabled(JObj, Default) ->
    kz_json:is_true(?KEY_IS_ENABLED, JObj, Default).

-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value(?KEY_IS_ENABLED, 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value(?KEY_IS_ENABLED, 'false', JObj).

-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

devices(UserJObj) ->
    AccountDb = kz_doc:account_db(UserJObj),
    UserId = kz_doc:id(UserJObj),

    ViewOptions = [{'startkey', [UserId]}
                   ,{'endkey', [UserId, kz_json:new()]}
                   ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"kz_attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [UserId, _R]),
            []
    end.

-spec fetch(ne_binary(), ne_binary()) -> {'ok', doc()} | {'error', any()}.
fetch(<<_/binary>> = AccountId, <<_/binary>> = UserId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, UserId);
fetch(_, _) ->
    {'error', 'invalid_parametres'}.

-spec fax_settings(doc()) -> doc().
fax_settings(JObj) ->
    FaxSettings = kz_json:get_json_value(?FAX_SETTINGS_KEY, JObj, kz_json:new()),
    UserFaxSettings = case kz_json:get_value(?FAX_TIMEZONE_KEY, FaxSettings) of
        'undefined' -> kz_json:set_value(?FAX_TIMEZONE_KEY, timezone(JObj), FaxSettings);
        _ -> FaxSettings
    end,
    AccountFaxSettings = kz_account:fax_settings(kz_doc:account_id(JObj)),
    kz_json:merge_jobjs(UserFaxSettings, AccountFaxSettings).
