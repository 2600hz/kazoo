%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_fax_util).

-export([add_data/1
        ,add_attachments/3
        ,to_email_addresses/2
        ]).

-include("teletype.hrl").

-define(CONVERT_CONFIG_CAT, <<"kazoo_convert">>).

-spec add_data(kz_json:object()) -> kz_json:object().
add_data(DataJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    FaxDoc = maybe_get_fax_doc(DataJObj, IsPreview),
    FaxBoxJObj = get_faxbox_doc(DataJObj, FaxDoc),
    Values =
        props:filter_empty(
          [{<<"error">>, error_data(DataJObj, IsPreview)}
          ,{<<"faxbox">>, FaxBoxJObj}
          ,{<<"owner">>, get_owner_doc(DataJObj, FaxBoxJObj)}
          ,{<<"fax_doc">>, FaxDoc}
          ,{<<"timezone">>, find_timezone(DataJObj, FaxBoxJObj)}
          ]),
    kz_json:set_values(Values, DataJObj).

-spec add_attachments(kz_json:object(), kz_term:proplist(), boolean()) -> {kz_term:proplist(), attachments()}.
add_attachments(DataJObj, Macros, ShouldTerminate) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    FaxDoc = kz_json:get_value(<<"fax_doc">>, DataJObj),
    case kz_json:is_json_object(FaxDoc)
        andalso not kz_json:is_empty(FaxDoc)
        andalso maybe_fetch_attachments(DataJObj, FaxDoc, Macros, IsPreview)
    of
        'false' -> maybe_terminate(Macros, ShouldTerminate, IsPreview);
        [] -> maybe_terminate(Macros, ShouldTerminate, IsPreview);
        Attachments -> {add_document_data(FaxDoc, Macros, Attachments), Attachments}
    end.

-spec maybe_terminate(kz_term:proplist(), boolean(), boolean()) -> {kz_term:proplist(), attachments()}.
maybe_terminate(Macros, _, 'true') ->
    lager:debug("this is a preview, no attachments"),
    {Macros, []};
maybe_terminate(_, 'true', 'false') ->
    lager:debug("no attachments were found for this fax"),
    throw({'error', 'no_attachment'});
maybe_terminate(Macros, 'false', 'false') ->
    lager:debug("no attachments were found for this fax"),
    {Macros, []}.

-spec add_document_data(kz_json:object(), kz_term:proplist(), attachments()) -> kz_term:proplist().
add_document_data(FaxDoc, Macros, [{ContentType, Filename, Bin}]) ->
    FaxDocProps = kz_json:to_proplist(kz_doc:public_fields(FaxDoc)),
    Values =
        props:filter_undefined(
          [{<<"media">>, Filename}
          ,{<<"document_type">>, kz_mime:to_extension(ContentType)}
          ,{<<"document_size">>, erlang:size(Bin)}
           | FaxDocProps
          ]),
    FaxMacros = props:set_values(Values, props:get_value(<<"fax">>, Macros, [])),
    props:set_value(<<"fax">>, FaxMacros, Macros).

-spec to_email_addresses(kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binaries().
to_email_addresses(DataJObj, TemplateId) ->
    BoxEmailPath = case TemplateId of
                       <<"fax_inbound", _/binary>> ->
                           [<<"notifications">>, <<"inbound">>, <<"email">>, <<"send_to">>]; %% inbound from faxbox doc
                       _ ->
                           [<<"notifications">>, <<"outbound">>, <<"email">>, <<"send_to">>] %% outbound from faxbox doc
                   end,
    Paths = [[<<"to">>, <<"email_addresses">>] %% explicitly set in the payload
            ,[<<"to">>] %% explicitly set in the payload in another form
            ,[<<"fax">>, <<"email">>, <<"send_to">>] %% fax document legacy
            ,[<<"fax">>, <<"notifications">>, <<"email">>, <<"send_to">>] %% fax document
            ,[<<"fax_notifications">>, <<"email">>, <<"send_to">>] %% set by fax_worker from faxbox doc
            ,[<<"notifications">>, <<"email">>, <<"send_to">>] %% faxbox or fax doc?
            ,BoxEmailPath
            ,[<<"owner">>, <<"email">>] %% user document
            ,[<<"owner">>, <<"username">>] %% user document
            ],
    Emails = kz_json:get_first_defined(Paths, DataJObj),
    Found = to_email_addresses(DataJObj, TemplateId, Emails),
    lager:debug("found emails: ~p", [Found]),
    Found.

-spec to_email_addresses(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary() | kz_term:api_binaries()) -> kz_term:api_binaries().
to_email_addresses(_, _, ?NE_BINARY=Email) ->
    [Email];
to_email_addresses(_, _, Emails)
  when is_list(Emails)
       andalso length(Emails) > 0 ->
    Emails;
to_email_addresses(_DataJObj, TemplateId, _) ->
    lager:debug("can not find email address for the fax notification, maybe using defaults"),
    case teletype_util:template_system_value(TemplateId, <<"default_to">>) of
        'undefined' -> 'undefined';
        <<Email/binary>> -> [Email];
        Emails when is_list(Emails) -> Emails
    end.

%%%=============================================================================
%%% Build data functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_faxbox_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_faxbox_doc(DataJObj, FaxDoc) ->
    BoxId = kz_json:find(<<"faxbox_id">>, [DataJObj, FaxDoc]),
    case kz_term:is_ne_binary(BoxId)
        andalso teletype_util:open_doc(<<"faxbox">>, BoxId, DataJObj)
    of
        'false' -> kz_json:new();
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec error_data(kz_json:object(), boolean()) -> kz_json:object().
error_data(DataJObj, 'false') ->
    kz_json:from_list(
      [{<<"call_info">>, kz_json:get_value(<<"fax_error">>, DataJObj)}
      ,{<<"fax_info">>, kz_json:get_value([<<"fax_info">>, <<"fax_result_text">>], DataJObj)}
      ]);
error_data(_, 'true') ->
    kz_json:from_list(
      [{<<"call_info">>, <<"CALL_INFO">>}
      ,{<<"fax_info">>, <<"FAX_INFO">>}
      ]).

-spec get_owner_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_owner_doc(DataJObj, FaxBoxJObj) ->
    OwnerId = kzd_fax_box:owner_id(FaxBoxJObj, kz_json:get_value(<<"owner_id">>, DataJObj)),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec maybe_get_fax_doc(kz_json:object(), boolean()) -> kz_json:object().
maybe_get_fax_doc(DataJObj, 'true') ->
    case teletype_util:open_doc(<<"fax">>, 'undefined', DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> kz_json:new()
    end;
maybe_get_fax_doc(DataJObj, 'false') ->
    FaxId = kz_json:get_ne_binary_value(<<"fax_id">>, DataJObj),
    case get_fax_doc(fax_db(DataJObj, FaxId), FaxId) of
        {'ok', JObj} -> JObj;
        {'error', _} -> kz_json:new()
    end.

-spec get_fax_doc(kz_term:ne_binary(), kz_term:api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_fax_doc(_, 'undefined') ->
    lager:debug("undefined fax_id"),
    {'error', 'not_found'};
get_fax_doc(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, {kzd_fax:type(), Id}) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} when Db =/= ?KZ_FAXES_DB ->
            get_fax_doc(?KZ_FAXES_DB, Id);
        {'error', _Reason}=Error ->
            lager:debug("failed to open fax ~s/~s document: ~p", [Db, Id, _Reason]),
            Error
    end.

-spec find_timezone(kz_term:api_ne_binary() | kz_json:object(), kz_json:object()) -> kz_term:ne_binary().
find_timezone('undefined', FaxBoxJObj) ->
    kzd_fax_box:timezone(FaxBoxJObj);
find_timezone(Timezone, _FaxBoxJObj) when is_binary(Timezone) ->
    Timezone;
find_timezone(DataJObj, FaxBoxJObj) ->
    Paths = [<<"fax_timezone">>
            ,[<<"fax_info">>, <<"fax_timezone">>]
            ,[<<"fax">>, <<"tx_result">>, <<"timezone">>]
            ,[<<"fax">>, <<"rx_result">>, <<"timezone">>]
            ],
    find_timezone(kz_json:get_first_defined(Paths, DataJObj), FaxBoxJObj).

%%%=============================================================================
%%% Attachment Utilities
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fax_db(kz_json:object(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
fax_db(DataJObj, FaxId) ->
    case kapi_notifications:account_db(DataJObj, 'true') of
        'undefined' ->
            maybe_get_fax_db_from_id(kz_json:get_ne_binary_value(<<"account_id">>, DataJObj), FaxId);
        Db -> maybe_get_fax_db_from_id(Db, FaxId)
    end.

-spec maybe_get_fax_db_from_id(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
maybe_get_fax_db_from_id('undefined', _) -> ?KZ_FAXES_DB;
maybe_get_fax_db_from_id(?MATCH_MODB_SUFFIX_ENCODED(_, _, _)=Db, _) -> Db;
maybe_get_fax_db_from_id(Db, ?MATCH_MODB_PREFIX(Year, Month, _)) -> kazoo_modb:get_modb(kzs_util:format_account_id(Db), Year, Month);
maybe_get_fax_db_from_id(Db, _) -> Db.

-spec maybe_fetch_attachments(kz_json:object(), kz_json:object(), kz_term:proplist(), boolean()) -> attachments().
maybe_fetch_attachments(_, _, _, 'true') ->
    [];
maybe_fetch_attachments(DataJObj, FaxJObj, Macros, 'false') ->
    FaxId = kz_doc:id(FaxJObj),
    Db = kz_doc:account_db(FaxJObj),
    lager:debug("accessing fax attachment ~s at ~s", [Db, FaxId]),
    teletype_util:send_update(DataJObj, <<"pending">>),
    Format = kapps_config:get_ne_binary(?CONVERT_CONFIG_CAT, [<<"fax">>, <<"attachment_format">>], <<"pdf">>),
    case kz_fax_attachment:fetch(Format, Db, FaxJObj) of
        {'ok', Content, ContentType, _Doc} ->
            Filename = get_file_name(Macros, get_extension(Format, ContentType)),
            [{ContentType, Filename, Content}];
        {'error', _E} ->
            lager:debug("failed to fetch attachment: ~p", [_E]),
            []
    end.

-spec get_extension(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_extension(<<"original">>, ContentType) -> kz_mime:to_extension(ContentType);
get_extension(Format, _) -> Format.

-spec get_file_name(kz_term:proplist(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_file_name(Macros, Ext) ->
    CallerIdMacros = props:get_value(<<"caller_id">>, Macros),
    CallerID =
        case {props:get_value(<<"name">>, CallerIdMacros)
             ,props:get_value(<<"number">>, CallerIdMacros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> kz_term:to_binary(Num);
            {Name, _} -> kz_term:to_binary(Name)
        end,
    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_time:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_term:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).
