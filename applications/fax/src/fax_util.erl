%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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
-export([content_type_to_extension/1, extension_to_content_type/1]).
-export([notify_email_list/3]).
-export([filter_numbers/1]).
-export([is_valid_caller_id/2]).

-include("fax.hrl").

-spec fax_properties(wh_json:object()) -> wh_proplist().
fax_properties(JObj) ->
    [{wh_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(JObj)].

-spec collect_channel_props(wh_json:object()) ->
                                   wh_proplist().
-spec collect_channel_props(wh_json:object(), wh_proplist() | ne_binaries()) ->
                                   wh_proplist().
-spec collect_channel_props(wh_json:object(), wh_proplist() | ne_binaries(), wh_proplist()) ->
                                   wh_proplist().
collect_channel_props(JObj) ->
    collect_channel_props(JObj, ?FAX_CHANNEL_DESTROY_PROPS).

collect_channel_props(JObj, List) ->
    collect_channel_props(JObj, List, []).

collect_channel_props(JObj, List, Acc) ->
    lists:foldl(fun({Key, Keys}, Acc0) ->
                        collect_channel_props(wh_json:get_value(Key, JObj), Keys, Acc0);
                   (Key, Acc0) ->
                        [collect_channel_prop(Key, JObj) | Acc0]
                end, Acc, List).

-spec collect_channel_prop(ne_binary(), wh_json:object()) ->
                                  {wh_json:key(), wh_json:json_term()}.
collect_channel_prop(<<"Hangup-Code">> = Key, JObj) ->
    <<"sip:", Code/binary>> = wh_json:get_value(Key, JObj, <<"sip:500">>),
    {Key, Code};
collect_channel_prop(Key, JObj) ->
    {Key, wh_json:get_value(Key, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(ne_binary() | string() | list()) -> ne_binary().
content_type_to_extension(CT) when not is_binary(CT) ->
    content_type_to_extension(wh_util:to_binary(CT));
content_type_to_extension(CT) when is_binary(CT) ->
    Cmd = binary_to_list(<<"echo -n `grep -E '^", CT/binary, "\\s' /etc/mime.types "
                           "2> /dev/null "
                           "| head -n1 "
                           "| awk '{print $2}'`">>),
    case os:cmd(Cmd) of
        [] ->
            lager:debug("content-type ~s not handled, returning 'tmp'",[CT]),
            <<"tmp">>;
        Ext -> wh_util:to_binary(Ext)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known extensions to media types
%% @end
%%--------------------------------------------------------------------
-spec extension_to_content_type(ne_binary() | string() | list()) -> ne_binary().
extension_to_content_type(Ext) when not is_binary(Ext) ->
    extension_to_content_type(wh_util:to_binary(Ext));
extension_to_content_type(<<".", Ext/binary>>) ->
    extension_to_content_type(Ext);
extension_to_content_type(Ext) when is_binary(Ext) ->
    Cmd = binary_to_list(<<"echo -n `grep -E '\\s", Ext/binary, "($|\\s)' /etc/mime.types "
                           "2> /dev/null "
                           "| head -n1 "
                           "| cut -f1`">>),
    case os:cmd(Cmd) of
        "" ->
            lager:debug("extension ~s not handled, returning 'application/octet-stream'",[Ext]),
            <<"application/octet-stream">>;
        CT -> CT
    end.


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
    case wh_util:is_empty(A) of
        'true' -> wh_util:to_hex_binary(crypto:rand_bytes(16));
        'false' -> A
    end.

-spec maybe_attach_extension(ne_binary(), ne_binary()) -> ne_binary().
maybe_attach_extension(A, CT) ->
    case wh_util:is_empty(filename:extension(A)) of
        'false' -> A;
        'true' -> <<A/binary, ".", (content_type_to_extension(CT))/binary>>
    end.

-spec save_fax_docs(wh_json:objects(), binary(), ne_binary()) ->
                           'ok' |
                           {'error', any()}.
save_fax_docs([], _FileContents, _CT) -> 'ok';
save_fax_docs([Doc|Docs], FileContents, CT) ->
    case couch_mgr:save_doc(?WH_FAXES_DB, Doc) of
        {'ok', JObj} ->
            case save_fax_attachment(JObj, FileContents, CT) of
                {'ok', _} -> save_fax_docs(Docs, FileContents, CT);
                Error -> Error
            end;
        Else -> Else
    end.

-spec save_fax_attachment(api_object(), binary(), ne_binary())->
                                 {'ok', wh_json:object()} |
                                 {'error', ne_binary()}.
-spec save_fax_attachment(api_object(), binary(), ne_binary(), non_neg_integer())->
                                 {'ok', wh_json:object()} |
                                 {'error', ne_binary()}.
save_fax_attachment(JObj, FileContents, CT) ->
    save_fax_attachment(JObj, FileContents, CT, whapps_config:get_integer(?CONFIG_CAT, <<"max_storage_retry">>, 5)).

save_fax_attachment(JObj, _FileContents, _CT, 0) ->
    DocId = wh_doc:id(JObj),
    Rev = wh_doc:revision(JObj),
    lager:debug("max retry saving attachment on fax id ~s rev ~s",[DocId, Rev]),
    {'error', <<"max retry saving attachment">>};
save_fax_attachment(JObj, FileContents, CT, Count) ->
    DocId = wh_doc:id(JObj),
    Rev = wh_doc:revision(JObj),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}
            ,{'rev', Rev}
           ],
    Name = attachment_name(<<>>, CT),
    _ = couch_mgr:put_attachment(?WH_FAXES_DB, DocId, Name, FileContents, Opts),
    case check_fax_attachment(DocId, Name) of
        {'ok', J} -> save_fax_doc_completed(J);
        {'missing', J} ->
            lager:debug("Missing fax attachment on fax id ~s rev ~s",[DocId, Rev]),
            save_fax_attachment(J, FileContents, CT, Count-1);
        {'error', _R} ->
            lager:debug("Error ~p saving fax attachment on fax id ~s rev ~s",[_R, DocId, Rev]),
            {'ok', J} = couch_mgr:open_doc(?WH_FAXES_DB, DocId),
            save_fax_attachment(J, FileContents, CT, Count-1)
    end.

-spec check_fax_attachment(ne_binary(), ne_binary())->
                                  {'ok', wh_json:object()} |
                                  {'missing', wh_json:object()} |
                                  {'error', any()}.
check_fax_attachment(DocId, Name) ->
    case couch_mgr:open_doc(?WH_FAXES_DB, DocId) of
        {'ok', JObj} ->
            case wh_doc:attachment(JObj, Name) of
                'undefined' -> {'missing', JObj};
                _Else -> {'ok', JObj}
            end;
        {'error', _}=E -> E
    end.

-spec save_fax_doc_completed(wh_json:object())->
                                    {'ok', wh_json:object()} |
                                    {'error', any()}.
save_fax_doc_completed(JObj)->
    DocId = wh_doc:id(JObj),
    case couch_mgr:save_doc(?WH_FAXES_DB, wh_json:set_values([{<<"pvt_job_status">>, <<"pending">>}], JObj)) of
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
is_valid_caller_id(Number, AccountId) ->
    case wh_number_manager:lookup_account_by_number(Number) of
        {'ok', AccountId, _} -> 'true';
        _Else -> 'false'
    end.

-spec is_digit(integer()) -> boolean().
is_digit(N) -> N >= $0 andalso N =< $9.
