{application, whistle_apps,
 [
  {description, "Applications on top of Whistle"}
  ,{id, "9fd3b140-8727-11e0-9d78-0800200c9a66"}
  ,{vsn, "0.5.0"}
  ,{modules, [
               whapps_controller
              ,whistle_util
              ,whistle_app
              ,whistle_cli
              ,whistle_deps
              ,whistles
              ,whistle_sup
            ]}
  ,{registered, [whapps_controller]}
  ,{applications, [
                   kernel
                   ,stdlib
                  ]}
  ,{included_applications, [
                             sasl
                            ,crypto
                            ,ibrowse
                            ,riak_err
                            ,whistle_amqp
                            ,whistle_couch
                           ]}
  ,{mod, {whistle_apps_app, []}}
  ,{env, [{reloader, false}]} % set to true to enable reloader
 ]}.
