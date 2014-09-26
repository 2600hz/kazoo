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
-export([content_type_to_extension/1]).
-export([notify_email_list/3]).

-include("fax.hrl").

-spec fax_properties(wh_json:object()) -> wh_proplist().
fax_properties(JObj) ->
    [{wh_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(JObj)].

-spec collect_channel_props(wh_json:object()) ->
                                   wh_proplist().
-spec collect_channel_props(wh_json:object(), wh_proplist()) ->
                                   wh_proplist().
-spec collect_channel_props(wh_json:object(), wh_proplist(), wh_proplist()) ->
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
    <<"sip:", Code/binary>> = wh_json:get_value(Key, JObj),
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
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"image/tiff">>) -> <<"tiff">>;
content_type_to_extension(CT) when is_binary(CT) ->
    lager:debug("content-type ~s not handled, returning 'tmp'",[CT]),
    <<"tmp">>.


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

-spec save_fax_docs(api_objects(), binary(), ne_binary())-> 'ok' | 'error'.
save_fax_docs([],_FileContents, _CT) -> 'ok';
save_fax_docs([Doc|Docs], FileContents, CT) ->
    case couch_mgr:save_doc(?WH_FAXES, Doc) of
        {'ok', JObj} ->
            save_fax_attachment(JObj, FileContents, CT),
            save_fax_docs(Docs, FileContents, CT);
        _Else -> 'error'
    end.

-spec save_fax_attachment(api_object(), binary(), ne_binary(), integer())-> {'ok', wh_json:object()} | {'error', any()}.
-spec save_fax_attachment(api_object(), binary(), ne_binary())-> {'ok', wh_json:object()} | {'error', any()}.
save_fax_attachment(JObj, FileContents, CT) ->
    save_fax_attachment(JObj, FileContents, CT, whapps_config:get_integer(?CONFIG_CAT, <<"max_storage_retry">>, 5)).

save_fax_attachment(JObj, _FileContents, _CT, 0) ->
    DocId = wh_json:get_value(<<"_id">>, JObj),
    Rev = wh_json:get_value(<<"_rev">>, JObj),
    lager:debug("max retry saving attachment on fax id ~s rev ~s",[DocId, Rev]),
    {'error', <<"max retry saving attachment">>};
save_fax_attachment(JObj, FileContents, CT, Count) ->
    DocId = wh_json:get_value(<<"_id">>, JObj),
    Rev = wh_json:get_value(<<"_rev">>, JObj),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}
            ,{'rev', Rev}
           ],
    Name = attachment_name(<<>>, CT),
    case couch_mgr:put_attachment(?WH_FAXES, DocId, Name, FileContents, Opts) of
        {'ok', _DocObj} ->
            save_fax_doc_completed(DocId);
        {'error', E} ->
            lager:debug("Error ~p saving fax attachment on fax id ~s rev ~s",[E, DocId, Rev]),
            save_fax_attachment(JObj, FileContents, CT, Count-1)
    end.

-spec save_fax_doc_completed(ne_binary())-> {'ok', wh_json:object()} | {'error', any()}.
save_fax_doc_completed(DocId)->
    case couch_mgr:open_doc(?WH_FAXES, DocId) of
        {'error', E} ->
            lager:debug("error ~p reading fax ~s while setting to pending",[E, DocId]),
            {'error', E};
        {'ok', JObj} ->
            case couch_mgr:save_doc(?WH_FAXES, wh_json:set_values([{<<"pvt_job_status">>, <<"pending">>}], JObj)) of
                {'ok', Doc} ->
                    lager:debug("fax jobid ~s set to pending", [DocId]),
                    {'ok', Doc};
                {'error', E} ->
                    lager:debug("error ~p setting fax jobid ~s to pending",[E, DocId]),
                    {'error', E}
            end
    end.

-spec notify_email_list(ne_binary(), api_binary(), list()) -> list().
notify_email_list(From, 'undefined', List) ->
    lists:usort([From | List]);
notify_email_list(From, OwnerEmail, List) ->
    lists:usort([From, OwnerEmail | List]).
