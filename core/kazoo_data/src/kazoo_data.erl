%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_data).

-include("kz_data.hrl").

-export_type([db_create_options/0
             ,data_error/0
             ,data_errors/0
             ,get_results_return/0
             ,db_classification/0
             ,view_options/0, key_range/0
             ,docid/0, docids/0
             ]).
