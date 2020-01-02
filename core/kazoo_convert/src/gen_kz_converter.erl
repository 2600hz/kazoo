%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Behavior for File converter modules.
%%%
%%% Defines the behavior for file format converters in the kazoo_convert library.
%%%
%%% Converters:
%%% <ul>
%%%   <li>The behavior `gen_kz_converter' specifies the interface of the functions convert/4 and read_metadata/1, and define their return types.</li>
%%%   <li>Converter modules should always delete any files created in the process,
%%%   including the input file if the {'file', FilePath} `Content' format is specified. If `output_type' is `path' the file converted file will be returned and deletion of this file will be
%%%   the responsibility of the caller. </li>
%%%   <li>`binary' and `path' formats in the requested `output_type' must be supported.</li>
%%%   <li>Input content formats `{file, FilePath}' and a binary containing the files content must be supported.</li>
%%%   <li>Any files created in the process should be stored in the tmp_dir parameter, the configured `tmp_dir` or `/tmp' by default.</li>
%%% </ul>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_kz_converter).

-type converted() :: {'ok', any()}|
                     {'ok', any(), kz_term:proplist()}|
                     {'error', any()}.

-callback convert(kz_term:api_binary()
                 ,kz_term:api_binary()
                 ,any()
                 ,kz_term:proplist()) -> any().

-callback read_metadata(kz_term:ne_binary()) -> kz_term:proplist().

-export_type([converted/0]).
