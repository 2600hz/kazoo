{application, webmachine,
 [{description, "webmachine"},
  {vsn, "1.8.0"},
  {modules, [
    webmachine,
    webmachine_app,
    webmachine_decision_core,
    webmachine_deps,
    webmachine_dispatcher,
    webmachine_error_handler,
    webmachine_logger,
    webmachine_perf_logger,
    webmachine_resource,
    webmachine_request,
    webmachine_router,
    webmachine_sup,
    webmachine_mochiweb,
    webmachine_multipart,
    webmachine_util,
    wrq,
    wmtrace_resource
  ]},
  {registered, []},
  {mod, {webmachine_app, []}},
  {env, [
         {dispatch_list, []}
        ]},
  {applications, [kernel, stdlib, crypto]}]}.
