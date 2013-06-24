{application, whistle_config
 ,[
   {description, "Kazoo Configuration Helper"}
   ,{vsn, "1.0.0"}
   ,{modules, [wh_config]}
   ,{registered, []}
   ,{applications
     ,[
       kernel
       ,stdlib
       ,crypto
       ,sasl
      ]}
   ,{mod, { whistle_config_app
            ,[]
          }
    }
   ,{env, []}
 ]}.
