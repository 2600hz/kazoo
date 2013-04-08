%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.


%% @doc This module contains utilities to manage attachments

-module(couchbeam_attachments).

-include_lib("couchbeam/include/couchbeam.hrl").

-export([wait_for_attachment/2, attachment_acceptor/3]).
-export([add_inline/3, add_inline/4,
        delete_inline/2]).

%% @doc collect all attachments chunks
wait_for_attachment(ReqId, Timeout) ->
    wait_for_attachment(ReqId, Timeout, []).

wait_for_attachment(ReqId, Timeout, Acc) ->
    receive
        {ReqId, done} ->
            {ok, iolist_to_binary(lists:reverse(Acc))};
        {ReqId, {ok, Data}} ->
            wait_for_attachment(ReqId, Timeout, [Data|Acc]);

        {ReqId, {error, Reason}} ->
            case Reason of
            {"404", _} ->
                {error, not_found};
            {"412", _} ->
                {error, precondition_failed};
            _ ->
                {error, Reason}
            end
    after Timeout ->
        {error, {timeout, Acc}}
    end.

%% @doc initiate attachment fetching
attachment_acceptor(Pid, PidRef, Timeout) ->
    receive
        {ibrowse_req_id, PidRef, IbrowseRef} ->
            attachment_acceptor(Pid, PidRef, Timeout, IbrowseRef)
    after Timeout ->
        Pid ! {PidRef, {error, {timeout, []}}}
    end.

%% @doc main ibrowse loop to fetch attachments
attachment_acceptor(Pid, PidRef, Timeout, IbrowseRef) ->
    receive
        {ibrowse_async_response_end, IbrowseRef} ->
            Pid ! {PidRef, done};
        {ibrowse_async_response, IbrowseRef, {error,Error}} ->
            Pid ! {PidRef, {error, Error}};
        {ibrowse_async_response, IbrowseRef, Data} ->
            Pid ! {PidRef, {ok, Data}},
            attachment_acceptor(Pid, PidRef, Timeout, IbrowseRef),
            ibrowse:stream_next(IbrowseRef);

        {ibrowse_async_headers, IbrowseRef, Status, Headers} ->
            if Status =/= "200" ->
                    Pid ! {PidRef, {error, {Status, Headers}}};
                true ->
                    ibrowse:stream_next(IbrowseRef),
                    attachment_acceptor(Pid, PidRef, Timeout, IbrowseRef)
            end
    after Timeout ->
        Pid ! {PidRef, {error, timeout}}
    end.

%% @spec add_inline(Doc::json_obj(),Content::attachment_content(),
%%      AName::string()) -> json_obj()
%% @doc add attachment  to a doc and encode it. Give possibility to send attachments inline.
add_inline(Doc, Content, AName) ->
    ContentType = mochiweb_util:guess_mime(AName),
    add_inline(Doc, Content, AName, ContentType).

%% @spec add_inline(Doc::json_obj(), Content::attachment_content(),
%%      AName::string(), ContentType::string()) -> json_obj()
%% @doc add attachment  to a doc and encode it with ContentType fixed.
add_inline(Doc, Content, AName, ContentType) ->
    {Props} = Doc,
    Data = base64:encode(Content),
    Attachment = {couchbeam_util:to_binary(AName), {[{<<"content_type">>,
        couchbeam_util:to_binary(ContentType)}, {<<"data">>, Data}]}},

    Attachments1 = case proplists:get_value(<<"_attachments">>, Props) of
        undefined ->
            [Attachment];
        {Attachments} ->
            case set_attachment(Attachments, [], Attachment) of
                notfound ->
                    [Attachment|Attachments];
                A ->
                    A
                end
        end,
    couchbeam_doc:set_value(<<"_attachments">>, {Attachments1}, Doc).

%% @spec delete_inline(Doc::json_obj(), AName::string()) -> json_obj()
%% @doc delete an attachment record in doc. This is different from delete_attachment
%%      change is only applied in Doc object. Save_doc should be save to save changes.
delete_inline(Doc, AName) when is_list(AName) ->
    delete_inline(Doc, list_to_binary(AName));
delete_inline(Doc, AName) when is_binary(AName) ->
    {Props} = Doc,
    case proplists:get_value(<<"_attachments">>, Props) of
        undefined ->
            Doc;
        {Attachments} ->
            case proplists:get_value(AName, Attachments) of
                undefined ->
                    Doc;
                _ ->
                    Attachments1 = proplists:delete(AName, Attachments),
                    couchbeam_doc:set_value(<<"_attachments">>, {Attachments1}, Doc)
                end
        end.

% @private
set_attachment(Attachments, NewAttachments, Attachment) ->
    set_attachment(Attachments, NewAttachments, Attachment, false).
set_attachment([], Attachments, _Attachment, Found) ->
    case Found of
        true ->
            Attachments;
        false ->
            notfound
        end;
set_attachment([{Name, V}|T], Attachments, Attachment, Found) ->
    {AName, _} = Attachment,
    {Attachment1, Found1} = if
        Name =:= AName, Found =:= false ->
            {Attachment, true};
        true ->
            {{Name, V}, Found}
        end,
    set_attachment(T, [Attachment1|Attachments], Attachment, Found1).
