%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_doc).

-export([single_attachment_url/3]).

-include("kazoo_media.hrl").

-spec single_attachment_url(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
single_attachment_url(AccountDb, MediaId, AttachmentName) ->
    kz_datamgr:attachment_url(AccountDb, MediaId, AttachmentName).
