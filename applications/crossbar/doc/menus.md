

Menus, IVRs, what ever you call them, allow you to create branches in the callflow based on the caller's input.

The DTMF entered is matched against the "children" keys and that branch is taken.

Additionally, you can branch based on a timeout (no DTMF entered) by using "timeout" in the "children" keys":

    {"module":"menu"
     ,"data":{...}
     ,"children":{
       "1":{"module":"...",...}
       ,"2":{"module":"...",...}
       "timeout":{"module":"...",...}
     }

If no "timeout" child is specified, the menu is retried (until retries are exceeded).
