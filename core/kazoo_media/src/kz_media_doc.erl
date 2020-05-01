%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_doc).

-export([single_attachment_url/3]).

-include("kazoo_media.hrl").

-spec single_attachment_url(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
single_attachment_url(AccountDb, MediaId, AttachmentName) ->
    kz_datamgr:attachment_url(AccountDb, MediaId, AttachmentName).
