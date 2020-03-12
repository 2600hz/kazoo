-ifndef(KAZOO_DBG_HRL).

%% The DBG macros will trace function calls (along with parameters) and output them to the console
%% You can trace as many MFA combinations as necessary
%%
%% To trace all function calls made in the 'my_module' module:
%% some_test() ->
%%   ?DBG_START,
%%   ?DBG_TRACE('my_module'),
%%   Result = my_module:do_stuff(),
%%   ?DBG_STOP,
%%   ?assert(Result).

-define(DBG_START, dbg:start(), dbg:tracer()).
-define(DBG_TRACE(Module), dbg:tpl(Module, [{'_', [], [$_]}]), dbg:p('all', 'c')).
-define(DBG_TRACE(Module, Function), dbg:tpl({Module, Function, '_'}, [{'_', [], [$_]}]), dbg:p('all', 'c')).
-define(DBG_TRACE(Module, Function, Arity), dbg:tpl({Module, Function, Arity}, [{'_', [], [$_]}]), dbg:p('all', 'c')).
-define(DBG_STOP, dbg:stop_clear(),dbg:stop()).

-define(KAZOO_DBG_HRL, 'true').
-endif.
