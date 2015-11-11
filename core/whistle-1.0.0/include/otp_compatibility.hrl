-ifndef(OTP_COMPATIBILITY_INCLUDED).

- ifdef(OTP_AT_LEAST_18).
-  type array() :: array:array().
-  type dict() :: dict:dict().
-  type digraph() :: digraph:digraph().
-  type gb_set() :: gb_sets:set().
-  type gb_tree() :: gb_trees:tree().
-  type queue() :: queue:queue().
-  type set() :: sets:set().
- else.
- endif.

- define(OTP_COMPATIBILITY_INCLUDED, 'true').
-endif.
