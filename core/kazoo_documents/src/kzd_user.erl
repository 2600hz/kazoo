%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2022, 2600Hz
%%% @doc Device document manipulation
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_user).

-export([fetch/2]).
-export([email/1, email/2
        ,voicemail_notification_enabled/1, voicemail_notification_enabled/2
        ,to_vcard/1
        ,timezone/1, timezone/2
        ,presence_id/1, presence_id/2, set_presence_id/2
        ,is_enabled/1, is_enabled/2
        ,enable/1, disable/1
        ,type/0
        ,devices/1
        ,fax_settings/1
        ,name/1, first_name/1, last_name/1
        ,priv_level/1, priv_level/2, set_priv_level/2
        ,is_account_admin/1, is_account_admin/2

        ,call_restrictions/1, call_restrictions/2
        ,classifier_restriction/2, classifier_restriction/3, set_classifier_restriction/3
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-export_type([doc/0, docs/0]).

-define(KEY_EMAIL, <<"email">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_PRESENCE_ID, <<"presence_id">>).
-define(KEY_IS_ENABLED, <<"enabled">>).
-define(KEY_FIRST_NAME, <<"first_name">>).
-define(KEY_LAST_NAME, <<"last_name">>).
-define(KEY_PRIV_LEVEL, <<"priv_level">>).

-define(KEY_CALL_RESTRICTIONS, <<"call_restriction">>).
-define(KEY_CALL_RESTRICTION_ACTION, <<"action">>).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> {'ok', doc()} |
          {'error', any()}.
fetch(Account=?NE_BINARY, UserId=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccountDb, UserId, [{cache_failures,false}]);
fetch(_, _) ->
    {'error', 'invalid_parameters'}.

-spec email(doc()) -> kz_term:api_binary().
email(User) ->
    email(User, 'undefined').

-spec email(doc(), Default) -> kz_term:ne_binary() | Default.
email(User, Default) ->
    kz_json:get_ne_binary_value(?KEY_EMAIL, User, Default).

-spec voicemail_notification_enabled(doc()) -> boolean().
voicemail_notification_enabled(User) ->
    voicemail_notification_enabled(User, 'false').

-spec voicemail_notification_enabled(doc(), Default) -> boolean() | Default.
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
                     [kz_binary:join([X, Y], <<":">>) ||
                         {X, Y} <- NotEmptyFields
                     ]
                    ),
    DividedFields = lists:reverse(
                      lists:foldl(fun vcard_field_divide_by_length/2, [], PackedFields)
                     ),
    kz_binary:join(DividedFields, <<"\n">>).

-spec vcard_escape_chars(binary()) -> binary().
vcard_escape_chars(Val) ->
    Opts = ['global', {'return', 'binary'}],
    Val1 = re:replace(Val, "(:|;|,)", "\\\\&", Opts),
    re:replace(Val1, "\n", "\\\\n", Opts).

-spec vcard_fields_acc(vcard_field(), [{kz_term:ne_binary(), binary()}]) -> [{kz_term:ne_binary(), binary()}].
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

-spec vcard_normalize_val(binary() | {char(), kz_term:binaries()}) -> binary().
vcard_normalize_val({Separator, Vals}) when is_list(Vals) ->
    kz_binary:join([vcard_escape_chars(X) || X <- Vals, not kz_term:is_empty(X)], Separator);
vcard_normalize_val(Val) when is_binary(Val) ->
    vcard_escape_chars(Val).

-spec vcard_normalize_type(list() | {kz_term:ne_binary(), kz_term:ne_binary()} | kz_term:ne_binary()) -> kz_term:ne_binary().
vcard_normalize_type(T) when is_list(T) -> kz_binary:join([vcard_normalize_type(X) || X <- T], <<";">>);
vcard_normalize_type({T, V}) -> kz_binary:join([T, V], <<"=">>);
vcard_normalize_type(T) -> T.

-type vcard_val() :: binary() | {char(), kz_term:binaries()} | 'undefined'.
-type vcard_type_token() :: kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}.
-type vcard_type() :: [vcard_type_token()] | vcard_type_token().
-type vcard_field_token() :: {vcard_type(), vcard_val()}.
-type vcard_field() :: vcard_field_token() | [vcard_field_token()].

-spec card_field(kz_term:ne_binary(), kz_json:object()) -> vcard_field().
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
    ,kz_binary:join([X || X <- [FirstName, MiddleName, LastName],
                          not kz_term:is_empty(X)
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
    Addresses = [normalize_address(A) || A <- kz_json:get_value(<<"addresses">>, JObj, [])],
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

-spec content_type_to_type(kz_term:ne_binary()) -> kz_term:ne_binary().
content_type_to_type(<<"image/jpeg">>) -> <<"JPEG">>.

-spec vcard_field_divide_by_length(binary(), kz_term:binaries()) -> kz_term:binaries().
vcard_field_divide_by_length(<<Row:75/binary>>, Acc) ->
    [Row | Acc];
vcard_field_divide_by_length(<<Row:75/binary, Rest/binary>>, Acc) ->
    vcard_field_divide_by_length(Rest, [Row | Acc]);
vcard_field_divide_by_length(Row, Acc) ->
    [Row | Acc].

-spec normalize_address(kz_json:object()) -> {kz_term:ne_binary(), binary()}.
normalize_address(JObj) ->
    Types = case kz_json:get_value(<<"types">>, JObj) of
                'undefined' -> [<<"intl">>, <<"postal">>, <<"parcel">>, <<"work">>];
                T -> T
            end,
    Address = kz_json:get_value(<<"address">>, JObj),
    {kz_binary:join(Types, <<",">>), Address}.

-spec timezone(kz_json:object()) -> kz_term:api_binary().
timezone(JObj) ->
    timezone(JObj, 'undefined').

-spec timezone(kz_json:object(), Default) -> kz_term:ne_binary() | Default.
timezone(JObj, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, JObj, Default) of
        'undefined' -> kzd_accounts:timezone(kz_doc:account_id(JObj));
        <<"inherit">> -> kzd_accounts:timezone(kz_doc:account_id(JObj)); %% UI-1808
        TZ -> TZ
    end.

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(UserJObj) ->
    presence_id(UserJObj, 'undefined').

-spec presence_id(doc(), Default) -> kz_term:ne_binary() | Default.
presence_id(UserJObj, Default) ->
    kz_json:get_binary_value(?KEY_PRESENCE_ID, UserJObj, Default).

-spec set_presence_id(doc(), kz_term:ne_binary()) -> doc().
set_presence_id(UserJObj, Id) ->
    kz_json:set_value(?KEY_PRESENCE_ID
                     ,kz_term:to_binary(Id)
                     ,UserJObj
                     ).

-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    is_enabled(JObj, 'true').

-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(JObj, Default) ->
    kz_json:is_true(?KEY_IS_ENABLED, JObj, Default).

-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value(?KEY_IS_ENABLED, 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value(?KEY_IS_ENABLED, 'false', JObj).

-spec type() -> kz_term:ne_binary().
type() -> <<"user">>.

-spec devices(doc()) -> kzd_devices:docs().
devices(UserJObj) ->
    AccountDb = kz_doc:account_db(UserJObj),
    UserId = kz_doc:id(UserJObj),

    ViewOptions = [{'startkey', [UserId]}
                  ,{'endkey', [UserId, kz_json:new()]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [UserId, _R]),
            []
    end.


-spec is_account_admin(kz_term:api_object()) -> boolean().
is_account_admin('undefined') -> 'false';
is_account_admin(Doc) ->
    priv_level(Doc) =:= <<"admin">>.

-spec is_account_admin(kz_term:api_binary(), kz_term:api_binary()) -> boolean().
is_account_admin('undefined', _) -> 'false';
is_account_admin(_, 'undefined') -> 'false';
is_account_admin(Account, UserId) ->
    case fetch(Account, UserId) of
        {'ok', JObj} -> is_account_admin(JObj);
        {'error', _R} ->
            lager:debug("unable to open user ~s definition in account ~s: ~p", [UserId, Account, _R]),
            'false'
    end.

-spec fax_settings(doc()) -> doc().
fax_settings(JObj) ->
    FaxSettings = kz_json:get_json_value(?FAX_SETTINGS_KEY, JObj, kz_json:new()),
    UserFaxSettings = case kz_json:get_value(?FAX_TIMEZONE_KEY, FaxSettings) of
                          'undefined' -> kz_json:set_value(?FAX_TIMEZONE_KEY, timezone(JObj), FaxSettings);
                          _ -> FaxSettings
                      end,
    AccountFaxSettings = kzd_accounts:fax_settings(kz_doc:account_id(JObj)),
    kz_json:merge_jobjs(UserFaxSettings, AccountFaxSettings).

-spec name(doc()) -> kz_term:ne_binary().
name(Doc) ->
    list_to_binary([first_name(Doc, <<>>), " ", last_name(Doc, <<>>)]).

-spec first_name(doc()) -> kz_term:api_binary().
first_name(Doc) ->
    first_name(Doc, 'undefined').

-spec first_name(doc(), Default) -> kz_term:ne_binary() | Default.
first_name(Doc, Default) ->
    kz_json:get_binary_value(?KEY_FIRST_NAME, Doc, Default).

-spec last_name(doc()) -> kz_term:api_binary().
last_name(Doc) ->
    last_name(Doc, 'undefined').

-spec last_name(doc(), Default) -> kz_term:ne_binary() | Default.
last_name(Doc, Default) ->
    kz_json:get_binary_value(?KEY_LAST_NAME, Doc, Default).

-spec priv_level(doc()) -> kz_term:api_binary().
priv_level(Doc) ->
    priv_level(Doc, <<"user">>).

-spec priv_level(doc(), Default) -> kz_term:ne_binary() | Default.
priv_level(Doc, Default) ->
    kz_json:get_binary_value(?KEY_PRIV_LEVEL, Doc, Default).

-spec set_priv_level(kz_term:ne_binary(), doc()) -> doc().
set_priv_level(<<"user">>=LVL, Doc) ->
    kz_json:set_value(?KEY_PRIV_LEVEL, LVL, Doc);
set_priv_level(<<"admin">>=LVL, Doc) ->
    kz_json:set_value(?KEY_PRIV_LEVEL, LVL, Doc).

-spec call_restrictions(doc()) -> kz_term:api_object().
call_restrictions(Doc) ->
    call_restrictions(Doc, 'undefined').

-spec call_restrictions(doc(), Default) -> kz_json:object() | Default.
call_restrictions(Doc, Default) ->
    kz_json:get_json_value(?KEY_CALL_RESTRICTIONS, Doc, Default).

-spec classifier_restriction(doc(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
classifier_restriction(Doc, Classifier) ->
    classifier_restriction(Doc, Classifier, 'undefined').

-spec classifier_restriction(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | Default.
classifier_restriction(Doc, Classifier, Default) ->
    kz_json:get_ne_binary_value([?KEY_CALL_RESTRICTIONS
                                ,Classifier
                                ,?KEY_CALL_RESTRICTION_ACTION
                                ]
                               ,Doc
                               ,Default
                               ).

-spec set_classifier_restriction(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc().
set_classifier_restriction(Doc, Classifier, Action) ->
    kz_json:set_value([?KEY_CALL_RESTRICTIONS
                      ,Classifier
                      ,?KEY_CALL_RESTRICTION_ACTION
                      ]
                     ,Action
                     ,Doc
                     ).
