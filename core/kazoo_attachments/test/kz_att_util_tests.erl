%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_util_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOC_ID, "test_id").
-define(FIELD_KEY, "test_field").
-define(FIELD_VALUE, "test_value").
-define(ANAME, "test_att").
-define(ACCOUNT_ID, "483f6dc3c2b22d0db0da1fe19f2c3444").
-define(ACCOUNT_DB, "account/" ?ACCOUNT_ID).
-define(ACCOUNT_DB_ENC, "account%2F" ?ACCOUNT_ID).

-define(SEPARATOR, "/").

empty_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = [],

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<>>, URLSeg)].

default_format_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_util:default_format_url_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?ACCOUNT_ID, ?SEPARATOR
                    ,?DOC_ID, ?SEPARATOR
                    ,?ANAME
                   >>
                  ,URLSeg
                  )
    ].


azure_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_azure:azure_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?DOC_ID, "_", ?ANAME>>, URLSeg)].

dropbox_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_dropbox:dropbox_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?DOC_ID, "_", ?ANAME>>, URLSeg)].

gdrive_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_google_drive:gdrive_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?DOC_ID, "_", ?ANAME>>, URLSeg)].

gstorage_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_google_storage:gstorage_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?DOC_ID, "_", ?ANAME>>, URLSeg)].

onedrive_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_onedrive:onedrive_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?DOC_ID, "_", ?ANAME>>, URLSeg)].

s3_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = kz_att_s3:aws_default_fields(),

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?ACCOUNT_DB_ENC, ?SEPARATOR, ?DOC_ID, "_", ?ANAME>>, URLSeg)].

http_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = [{'const', <<"?path=">>}
             ,{'arg', <<"account_id">>}
             ,{'arg', <<"id">>}
             ,{'arg', <<"attachment">>}
             ],

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<"?path=", ?SEPARATOR, ?ACCOUNT_ID, ?SEPARATOR, ?DOC_ID, ?SEPARATOR, ?ANAME>>, URLSeg)].


field_url_test_() ->
    Args = [{<<"attachment">>, <<?ANAME>>}
           ,{<<"id">>, <<?DOC_ID>>}
           ,{<<"account_id">>, <<?ACCOUNT_ID>>}
           ,{<<"db">>, <<?ACCOUNT_DB>>}
           ],

    Metadata = kz_json:from_list([{<<"_id">>, <<?DOC_ID>>}
                                 ,{<<?FIELD_KEY>>, <<?FIELD_VALUE>>}
                                 ]),

    Format = [{'field', <<?FIELD_KEY>>}
             ,{'arg', <<"id">>}
             ,{'const', <<"foo">>}
             ,{'arg', <<"attachment">>}
             ],

    Params = #{'field_separator' => <<?SEPARATOR>>},

    URLSeg = kz_att_util:format_url(Params, Metadata, Args, Format),

    [?_assertEqual(<<?FIELD_VALUE, ?SEPARATOR, ?DOC_ID, ?SEPARATOR, "foo", ?SEPARATOR, ?ANAME>>, URLSeg)].
