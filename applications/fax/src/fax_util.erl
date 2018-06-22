%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_util).

-export([fax_properties/1]).
-export([collect_channel_props/1]).
-export([save_fax_docs/3, save_fax_attachment/3]).
-export([notify_email_list/3]).
-export([filter_numbers/1]).
-export([is_valid_caller_id/2]).
-export([normalize_content_type/1]).

-include("fax.hrl").

-spec fax_properties(kz_json:object()) -> kz_proplist().

fax_properties(JObj) ->
    [{kz_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- kz_json:to_proplist(JObj)].

-spec collect_channel_props(kz_json:object()) ->
                                   kz_proplist().
-spec collect_channel_props(kz_json:object(), kz_proplist() | ne_binaries()) ->
                                   kz_proplist().
-spec collect_channel_props(kz_json:object(), kz_proplist() | ne_binaries(), kz_proplist()) ->
                                   kz_proplist().
collect_channel_props(JObj) ->
    collect_channel_props(JObj, ?FAX_CHANNEL_DESTROY_PROPS).

collect_channel_props(JObj, List) ->
    collect_channel_props(JObj, List, []).

collect_channel_props(JObj, List, Acc) ->
    lists:foldl(fun({Key, Keys}, Acc0) ->
                        collect_channel_props(kz_json:get_value(Key, JObj), Keys, Acc0);
                   (Key, Acc0) ->
                        [collect_channel_prop(Key, JObj) | Acc0]
                end, Acc, List).

-spec collect_channel_prop(ne_binary(), kz_json:object()) ->
                                  {kz_json:path(), kz_json:json_term()}.
collect_channel_prop(<<"Hangup-Code">> = Key, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(Key, JObj, <<"sip:500">>),
    {Key, Code};
collect_channel_prop(Key, JObj) ->
    {Key, kz_json:get_value(Key, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun maybe_generate_random_filename/1
                 ,fun(A) -> maybe_attach_extension(A, CT) end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

-spec maybe_generate_random_filename(binary()) -> ne_binary().
maybe_generate_random_filename(A) ->
    case kz_term:is_empty(A) of
        'true' -> kz_term:to_hex_binary(crypto:strong_rand_bytes(16));
        'false' -> A
    end.

-spec maybe_attach_extension(ne_binary(), ne_binary()) -> ne_binary().
maybe_attach_extension(A, CT) ->
    case kz_term:is_empty(filename:extension(A)) of
        'false' -> A;
        'true' -> <<A/binary, ".", (kz_mime:to_extension(CT))/binary>>
    end.

-spec save_fax_docs(kz_json:objects(), binary(), ne_binary()) ->
                           'ok' |
                           {'error', any()}.
save_fax_docs([], _FileContents, _CT) -> 'ok';
save_fax_docs([Doc|Docs], FileContents, CT) ->
    case kz_datamgr:save_doc(?KZ_FAXES_DB, Doc) of
        {'ok', JObj} ->
            case save_fax_attachment(JObj, FileContents, CT) of
                {'ok', _} -> save_fax_docs(Docs, FileContents, CT);
                Error -> Error
            end;
        Else -> Else
    end.

-spec save_fax_attachment(api_object(), binary(), ne_binary())->
                                 {'ok', kz_json:object()} |
                                 {'error', ne_binary()}.
-spec save_fax_attachment(api_object(), binary(), ne_binary(), ne_binary(), non_neg_integer())->
                                 {'ok', kz_json:object()} |
                                 {'error', ne_binary()}.
save_fax_attachment(JObj, FileContents, CT) ->
    MaxStorageRetry = kapps_config:get_integer(?CONFIG_CAT, <<"max_storage_retry">>, 5),
    ContentsMD5 = kz_term:to_hex_binary(erlang:md5(FileContents)),
    Name = attachment_name(ContentsMD5, CT),

    save_fax_attachment(JObj, FileContents, CT, Name, MaxStorageRetry).

save_fax_attachment(JObj, _FileContents, _CT, _Name, 0) ->
    lager:error("max retry saving attachment ~s on fax id ~s rev ~s"
               ,[_Name, kz_doc:id(JObj), kz_doc:revision(JObj)]
               ),
    {'error', <<"max retry saving attachment">>};
save_fax_attachment(JObj, FileContents, CT, Name, Count) ->
    DocId = kz_doc:id(JObj),
    _ = attempt_save(JObj, FileContents, CT, Name),
    case check_fax_attachment(DocId, Name) of
        {'ok', J} -> save_fax_doc_completed(J);
        {'missing', J} ->
            lager:warning("missing fax attachment on fax id ~s",[DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            save_fax_attachment(J, FileContents, CT, Name, Count-1);
        {'error', _R} ->
            lager:debug("error '~p' saving fax attachment on fax id ~s",[_R, DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            {'ok', J} = kz_datamgr:open_doc(?KZ_FAXES_DB, DocId),
            save_fax_attachment(J, FileContents, CT, Name, Count-1)
    end.

-spec attempt_save(kz_json:object(), binary(), ne_binary(), ne_binary()) ->
                          {'ok', kz_json:objcet()} |
                          kz_datamgr:data_error().
attempt_save(JObj, FileContents, CT, Name) ->
    Opts = [{'content_type', CT}
           ],

    kz_datamgr:put_attachment(?KZ_FAXES_DB, kz_doc:id(JObj), Name, FileContents, Opts).

-spec check_fax_attachment(ne_binary(), ne_binary())->
                                  {'ok', kz_json:object()} |
                                  {'missing', kz_json:object()} |
                                  {'error', any()}.
check_fax_attachment(DocId, Name) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, DocId) of
        {'ok', JObj} ->
            case kz_doc:attachment(JObj, Name) of
                'undefined' -> {'missing', JObj};
                _Else -> {'ok', JObj}
            end;
        {'error', _}=E -> E
    end.

-spec save_fax_doc_completed(kz_json:object())->
                                    {'ok', kz_json:object()} |
                                    {'error', any()}.
save_fax_doc_completed(JObj)->
    DocId = kz_doc:id(JObj),
    Updates = [{<<"pvt_job_status">>, <<"pending">>}
              ,{<<"pvt_modified">>, kz_time:current_tstamp()}
              ],
    case kz_datamgr:save_doc(?KZ_FAXES_DB, kz_json:set_values(Updates, JObj)) of
        {'ok', Doc} ->
            lager:debug("fax jobid ~s set to pending", [DocId]),
            {'ok', Doc};
        {'error', E} ->
            lager:debug("error ~p setting fax jobid ~s to pending",[E, DocId]),
            {'error', E}
    end.

-spec notify_email_list(api_binary(), api_binary(), ne_binary() | list()) -> list().
notify_email_list(From, OwnerEmail, Email) when is_binary(Email) ->
    notify_email_list(From, OwnerEmail, [Email]);
notify_email_list('undefined', 'undefined', List) ->
    lists:usort(List);
notify_email_list(From, 'undefined', List) ->
    lists:usort([From | List]);
notify_email_list('undefined', OwnerEmail, List) ->
    lists:usort([OwnerEmail | List]);
notify_email_list(From, OwnerEmail, List) ->
    lists:usort([From, OwnerEmail | List]).

-spec filter_numbers(binary()) -> binary().
filter_numbers(Number) ->
    << <<X>> || <<X>> <= Number, is_digit(X)>>.

-spec is_valid_caller_id(api_binary(), ne_binary()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(<<>>, _) -> 'false';
is_valid_caller_id(Number, AccountId) ->
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, _} -> 'true';
        _Else -> 'false'
    end.

-spec is_digit(integer()) -> boolean().
is_digit(N) when is_integer(N),
                 N >= $0,
                 N =< $9 -> true;
is_digit(_) -> false.

-spec normalize_content_type(text()) -> ne_binary().
normalize_content_type(<<"image/tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/x-tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/x-tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"application/tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/x-tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/x-tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"application/pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"application/x-pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"text/pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"text/x-pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<_/binary>> = Else) -> Else;
normalize_content_type(CT) ->
    normalize_content_type(kz_term:to_binary(CT)).
