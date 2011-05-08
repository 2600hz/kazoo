{application, smtpc,
    [{description, "Erlmail SMTP Client application"},
     {vsn, "0.0.6"},
     {modules, [mail_mime,
                smtpc,
                smtpc_fsm
               ]},
     {env, [{smarthost, {{127,0,0,1},25}},
            {ehlo, "localhost"}]},
     {applications, [kernel, stdlib]}]}.
