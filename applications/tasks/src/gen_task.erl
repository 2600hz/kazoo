-module(gen_task).

-callback help(kz_json:object()) -> kz_json:object().
-callback help(kz_json:object(), Category) -> kz_json:object() when
      Category :: kz_term:ne_binary().
-callback help(kz_json:object(), Category, Action) -> kz_json:object() when
      Category :: kz_term:ne_binary(),
      Action :: kz_term:ne_binary().

%% @doc If the task creates an output CSV, this function returns the header cells
-callback output_header(TaskName, ExtraArgs) -> CellHeaders when
      TaskName :: kz_term:ne_binary(),
      ExtraArgs :: kz_tasks:extra_args(),
      CellHeaders :: kz_term:ne_binaries().

%% @doc Verify cell data for a header is valid
-callback cell_verifier(HeaderName, CellData) -> IsValidData when
      HeaderName :: kz_term:ne_binary(),
      CellData :: kz_json:json_term(),
      IsValidData :: boolean().

%% @doc `no_input` task executor
-callback execute(TaskName, ExtraArgs, Iterator) -> NewIterator when
      TaskName :: kz_term:ne_binary(),
      ExtraArgs :: kz_tasks:extra_args(),
      Iterator :: kz_tasks:iterator(),
      NewIterator :: kz_tasks:iterator().

%% @doc `input` task executor
-callback execute(TaskName, ExtraArgs, Iterator, CurrentInputRow) -> NewIterator when
      TaskName :: kz_term:ne_binary(),
      ExtraArgs :: kz_tasks:extra_args(),
      Iterator :: kz_tasks:iterator(),
      CurrentInputRow :: map(),
      NewIterator :: kz_tasks:iterator().

-optional_callbacks([cell_verifier/2
                    ,execute/3, execute/4
                    ]).
