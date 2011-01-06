{application,
  SKEL,
  [
    {description, "TODO"},
    {vsn, "VSN"},
    {modules, [SKEL_app,SKEL_sup]},
    {registered, [SKEL_sup]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {SKEL_app,[]}},
    {env, []}
 ]
}.
