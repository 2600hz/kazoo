
-record(error_info, {
          return = false,
          report = false,
          list = []
         }).

-record(dtl_context, {
          local_scopes = [], 
          block_dict = dict:new(), 
          trans_fun = none,
          trans_locales = [],
          auto_escape = [off],
          doc_root = "", 
          parse_trail = [],
          vars = [],
          const = [],
          record_info = [],
          filters = [],
          tags = [],
          libraries = [],
          custom_tags_dir = [],
          reader = {file, read_file},
          module = undefined,
          compiler_options = [],
          binary_strings = true,
          force_recompile = false,
          verbose = 0,
          is_compiling_dir = false,
          extension_module = undefined,
          scanner_module = erlydtl_scanner,
          scanned_tokens = [],
          all_options = [],
          errors = #error_info{},
          warnings = #error_info{},
          bin = undefined,
          lists_0_based = false,
          tuples_0_based = false
         }).

%% ALL fields of ast_info{} must be lists (see erlydtl_compiler_utils:merge_info/2)
-record(ast_info, {
          dependencies = [],
          translatable_strings = [],
          translated_blocks= [],
          custom_tags = [],
          var_names = [],
          def_names = [],
          const_names = []
         }).

-record(treewalker, {
          counter = 0,
          safe = false,
          extension = undefined,
          context
         }).    

-record(scanner_state, {
          template=[],
          scanned=[],
          pos={1,1},
          state=in_text
        }).


-define(ERR(Err, Ctx), erlydtl_compiler_utils:add_error(?MODULE, Err, Ctx)).
-define(WARN(Warn, Ctx), erlydtl_compiler_utils:add_warning(?MODULE, Warn, Ctx)).

-define(V_INFO,1).
-define(V_DEBUG,2).
-define(V_TRACE,3).

-define(LOG_INFO(Fmt, Args, Ctx), erlydtl_compiler_utils:print(?V_INFO, Fmt, Args, Ctx)).
-define(LOG_DEBUG(Fmt, Args, Ctx), erlydtl_compiler_utils:print(?V_DEBUG, Fmt, Args, Ctx)).
-define(LOG_TRACE(Fmt, Args, Ctx), erlydtl_compiler_utils:print(?V_TRACE, Fmt, Args, Ctx)).
