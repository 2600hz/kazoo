%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_doc).

-export([single_attachment_url/3]).

-include("kazoo_media.hrl").

-spec single_attachment_url(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
single_attachment_url(AccountDb, MediaId, AttachmentName) ->
    kz_datamgr:attachment_url(AccountDb, MediaId, AttachmentName).
