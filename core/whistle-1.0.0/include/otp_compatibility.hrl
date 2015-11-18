-ifndef(OTP_COMPATIBILITY_INCLUDED).

- ifdef(OTP_AT_LEAST_18).
-  type array:array() :: array:array().
-  type dict:dict() :: dict:dict().
-  type digraph:digraph() :: digraph:digraph().
-  type gb_sets:set() :: gb_sets:set().
-  type gb_trees:tree() :: gb_trees:tree().
-  type queue:queue() :: queue:queue().
-  type sets:set() :: sets:set().
- else.
- endif.

- define(OTP_COMPATIBILITY_INCLUDED, 'true').
-endif.
