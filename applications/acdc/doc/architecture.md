### Architecture

Agents represent the endpoints that a call in a queue will try to connect with.

#### Agents

Agents are comprised of two processes, a `gen_listener` (named process) and a `gen_fsm`. A supervisor ensures that the `gen_listener` and `gen_fsm` are kept alive together. A supervisor above that manages the list of agents.

> Agent process tree

```asciiart
 [ acdc_agents_sup ]
         |
         |--------[acdc_agent_sup]
         |--------[acdc_agent_sup]
                         |
                         |---[acdc_agent]
                         |---[acdc_agent_fsm]
```

> Agent FSM States

```asciiart
init -1-> sync -2-> ready -3-> ringing ---\
                     ^                     !
                     !---------4-----------!
                     7                     !
                  wrapup <-6- answered <-5-/
```

1. Agent process has started, trying to sync with other running agent processes
2. Agent process has synced (or timed out waiting) and has entered the *ready* state
3. Received a member_connect_win message
4. Failed to bridge to the agent
5. Bridged successfully to the agent
6. Call has unbridged/hungup
7. Wrapup timer has expired

#### Agent message processing

The interplay between AMQP messages, the agent process, and the agent FSM process

> Member Connect Requests

```asciiart
              AMQP                      Erlang
[Queue]                    [Agent]                     [AgentFSM]
                                                        {ready}
    -- member_connect_req  ->
                                -- member_connect_req  -> ok
                                <- member_connect_resp --
    <- member_connect_resp --
```

An agent will respond to all connect requests while in the *ready* state. The acdc_agent will pass along the `member_connect_req` to the FSM. If the FSM is in the *ready* state, the FSM will pass the request JSON to the agent process to send the `member_connect_resp` message. If the FSM is in any other state, the `member_connect_req` payload will be ignored.  

Depending on the Queue strategy (ring_all, round robin), 1 or more agents that responded may receive the `member_connect_win` message

> Member Connect Win and same Agent answers the member call

```asciiart
              AMQP                      Erlang
[Queue]                    [Agent]                     [AgentFSM]
                                                        {ready}
    -- member_connect_win -->
                                -- member_connect_win --> ok
                                <- bridge_member --------
                                                        {ringing}
                           {bind}
                                -- call_event ---------->
                           {bridged} ------------------->
                            <- member_connect_accepted --
  <- member_connect_accepted --
                                                        {answered}
                           {hangup} -------------------->
                           {unbind} <--------------------
                                                        {wrapup}
                                                                ---|
                                                                   | wrapup timeout
                                                                <--|
                                                        {ready}
```

> Member Connect Win and different Agent answers the member call

```asciiart
              AMQP                        Erlang
[Queue]                       [Agent]                     [AgentFSM]
                                                            {ready}
    ---- member_connect_win ---->
                                  -- member_connect_win --> ok
                                  <- bridge_member --------
                                                            {ringing}
                           {bind}
                                -- call_event ---------->
    -- member_connect_satisfied -->
                                  -- member_connect_satisfied -->
                                                            {ready}
```

When an agent receives a `member_connect_win` while in the *ready* state, the FSM will instruct the agent process to bridge to the agent's endpoint(s) and enter the *ringing* state. The agent process binds to the call events and attempts the bridge. Once the bridge is established, the FSM will instruct the agent process to send a `member_connect_accepted` and move to the *answered* state. On receiving a hangup event, the FSM will instruct the agent process to unbind from call events and will start the wrapup timer and enter the *wrapup* state. Once the timer fires, the FSM will return to the *ready* state. If another answers the member call then the agent process will receive a `member_connect_satisfied` and move back tp the *ready* state.

> Member Connect Win (fail to bridge)

```asciiart
              AMQP                      Erlang
[Queue]                    [Agent]                     [AgentFSM]
                                                        {ready}
    -- member_connect_win -->
                                -- member_connect_win --> ok
                                <- bridge_member --------
                                                        {ringing}
                           {bind}
                                -- call_event ---------->
                           {failed bridged} ------------>
                                <- member_connect_retry --
  <- member_connect_retry --
                           {unbind}
                                                        {ready}
```

When an agent is in the **ready** state, receives a `member_connect_win`, attempts to bridge, and the bridge fails, the agent FSM will instruct the agent process to send a `member_connect_retry`, unbind from the call events, and return the FSM to the **ready** state.

> Member Connect Win (not in **ready** state)

```asciiart
              AMQP                      Erlang
[Queue]                    [Agent]                     [AgentFSM]
                                                        {not_ready_state}
    -- member_connect_win -->
                                -- member_connect_win -->
                                <- member_connect_retry --
  <- member_connect_retry --
                           {unbind}
```

If a `member_connect_win` is received when __not__ in the **ready** state, reply with the `member_connect_retry` immediately.

#### Agents Manager

Agent login requires starting an agent process group; the Agents Manager process will take care of handling agent process group startup. Agent logout requires stopping the agent process group; Agents Manager will ensure the agent process group has terminated.

> Agents Manager process tree

```asciiart
[acdc_sup]
    |
    |----[acdc_agents_sup]
    |----[acdc_queues_sup]
    |----[acdc_agents_mgr]
```

##### Agents Manager message processing

* CF: Callflow action (for now, other mechanisms possible later)
* AsM: acdc_agents_mgr
* AsSup: acdc_agents_sup
* ASup: acdc_agent_sup
* APG: Agent Process Group - acdc_agent, acdc_agent_fsm

> Agent Login

```asciiart
            AMQP               Erlang
[CF]                 [AsM]                [AsSup]           [ASup]           [APG]
  -- agent_login ----->
     AcctId/AgentId
                       -- start_agent ------->
                          AcctId/AgentId
                                              -- start_child -->
                                                               -- start_link -->
```

> Agent Logout

```asciiart
            AMQP               Erlang
[CF]                 [AsM]                [AsSup]           [ASup]           [APG]
  -- agent_logout ----->
     AcctId/AgentId
                       -- stop_agent ------->
                          AcctId/AgentId
                                              -- terminate -->
                                                               -- terminate -->
```

> Agent Pause

```asciiart
            AMQP               Erlang
[CF]                 [AsM]                [AsSup]           [ASup]           [APG]
  -- agent_pause ----->
     AcctId/AgentId
                       -- pause_agent ------->
                          AcctId/AgentId
                                              -- pause -->
                                                               -- pause -->
```

> Agent Resume

```asciiart
            AMQP               Erlang
[CF]                 [AsM]                [AsSup]           [ASup]           [APG]
  -- agent_resume ----->
     AcctId/AgentId
                       -- pause_resume ------->
                          AcctId/AgentId
                                              -- resume -->
                                                               -- resume -->
```

#### Agent Stats

The various agent processes and FSMs will send stats to the Agents Manager process, which will record them to a Folsom table. Queries from interested parties (like Crossbar) will send AMQP requests which the Agent Manager will respond to as well.

#### Queues

A queue provides ordering of calls through it.

> Queue process tree

```asciiart
 [acdc_queues_sup]
          |
          |----[acdc_queue_sup]
          |----[acdc_queue_sup]
                       |
                       |----acdc_queue_manager
                       |----acdc_queue_workers_sup
                                 |
                                 |----acdc_queue_worker_sup
                                 |----acdc_queue_worker_sup
                                             |
                                             |----acdc_queue
                                             |----acdc_queue_shared
                                             |----acdc_queue_fsm
```

> Queue Startup

When `acdc_init` starts up (last child of acdc_sup), it iterates through each account. In each account, iterate over the queues available.

```asciiart
acdc_init:
  foreach acct in accts
    foreach queue in acct
      acdc_queues_sup:new(AccountId, QueueId)
```

`acdc_queues_sup` will start an `acdc_queue_sup` process to represent the AccountId/QueueId combo. The child list of the `acdc_queue_sup` has two entries: `acdc_queue_manager` and `acdc_queue_workers_sup`.

`acdc_queue_manager` handles receiving updates from config docs about the queue, member calls to the queue, etc.
`acdc_queue_workers_sup` handles managing actual member_call processing units. Its really a pool of `member_call workers`. This allows us to utilize AMQP's ack/nack features to handle worker crashes and not lose the member call, but still process multiple calls at once (meaning, if we have a queue with 10 agents, and 5 members call in, 5 phones had better start ringing).

To that end, when `acdc_queue_manager` has started up, it will find the ACDc queue's configuration and count how many configured agents that queue should have. This is the number of `acdc_queue_worker_sup` processes that will be started.

Note: since ACDc will run in multiple VMs, the total count of worker processes across the cluster will greatly outnumber the actual number of agents. This is okay for a couple reasons. One, queue BLF allows the caller to be connected to the next available call (a one-time agent of that queue). This should ensure that waiting member calls are actively processed and able to be routed to the temporary agent. One drawback is that AMQP likely round-robins in order of connection time, so if server 1 connects 10, then server 2 connects 10, the first ten calls are likely to be processed on server 1 while server 2 sits idle. Not sure its an issue, but could be problematic.

`acdc_queue_worker_sup` starts the three worker processes: `acdc_queue_listener`, `acdc_queue_fsm`, and `acdc_queue_shared`.
`acdc_queue_listener` is the AMQP arm of the worker, listening for and delivering AMQP payloads to the FSM, and sending AMQP payloads on behalf of the FSM.
`acdc_queue_fsm` is the brains of the worker. It tracks what state the member call is in, which agent to respond with a Win to, etc.
`acdc_queue_shared` is an AMQP listener for member_call payloads. It has the special restrictions of QoS of 1, and no auto-acking of payloads. When the FSM is finished with a member call, it is via `acdc_queue_shared` that the payload is ACK'd and the worker moves to the next call.

##### Queue - Shared

Because of the need to not auto-ack member call payloads, the consumption of messages from the shared AMQP queue necessitated moving that queue management into its own `gen_listener`, while messages betweem the queue process and agent processes flow unimpeded on the acdc_queue process's AMQP queue. The `acdc_queue_shared` routing is still passed to `acdc_queue_handlers`, to be fed into the `acdc_queue_fsm` process.

##### Queue FSM

> Queue FSM

```asciiart
 init ---> sync -1-> ready -2-> member_connect_req --\
                       ^                  4          3
                       \---5---- member_connecting --/
```

1. After init, queue is ready to process member_calls
2. member_call receieved from the shared AMQP queue
3. member_connect_resps have been received, timeout fired
4. member_connect_retry received from winning agent process
5. member_connect_accepted received from winning agent process

#### Queue message processing

The interplay between `member_call` requests (from callflow initially), the queue, queue fsm, and agent processes.

> Member Call Requests

```asciiart
            AMQP               Erlang                      AMQP                    AMQP
[CF]                 [Queue]             [QueueFSM]                  [Agents]                [Agent]
                                          {ready}
  -- member_call ----->
                         -- member_call -->
                       <- send_member_connect --
                     {bind}
                                       {member_conn_req}
                       ----------------------------- member_connect_req ->
                       <-- member_connect_resps --------------------------
                       -- member_connect_resps ->
                       <- send_conn_win --
                                       {member_conning}
                         ------------------------------------------------- member_connect_win ->
                         <- member_connect_accepted --------------------------------------------
                         -- accepted ---->
                         <- ACK member_call --
                     {unbind}
                                          {ready}
  <- ACK member_call ----
```

The queue receives a `member_call` payload from some source (probably a callflow action to start). The queue process passes the `member_call` to the FSM, which instructs the queue process to send the first `member_connect_req` payload. The queue process also binds to call events related to that member (looking for hangup).

The queue process sends the `member_connect_req`, and collects `member_connect_resp` payloads for a specified timeout. It hands the `member_connect_resps` to the FSM, which selects the winner and instructs the queue process to send the `member_connect_win` payload to the winning agent process.

Once the agent process has connected the caller to the agent endpoint, the queue process will receive a `member_connect_accepted` payload. The queue process hands this off to the FSM, which lets the queue process know it can ACK the member_call payload (and allow the AMQP queue to send the next `member_call` to the next consumer). The queue process unbinds from the call events.

> Member Call Requests (Agent Retry)

```asciiart
            AMQP               Erlang                      AMQP                    AMQP
[CF]                 [Queue]             [QueueFSM]                  [Agents]                [Agent]
                                          {ready}
  -- member_call ----->
                     {bind}
                         -- member_call -->
                       <- send_member_connect --
                                       {member_conn_req}
                       ----------------------------- member_connect_req ->
                       <-- member_connect_resps --------------------------
                       -- member_connect_resps ->
                       <- send_conn_win --
                                       {member_conning}
                         ------------------------------------------------- member_connect_win ->
                         <- member_connect_retry --------------------------------------------
                       -- member_connect_retry ->
                       <- send_member_connect --
                                       {member_conn_req}
```

This flow diagram shows how a queue reacts to a `member_connect_retry` (sent by the winning agent for a variety of reasons). The diagram loops back at the FSM re-entering the **member_conn_req** state.

> Member Call Requests (Call hungup)

```asciiart
            AMQP               Erlang                      AMQP                    AMQP
[Call]              [Queue]             [QueueFSM]                  [Agents]                [Agent]
                     {bind}
                                          {ready}
                         -- member_call -->
                       <- send_member_connect --
                                       {member_conn_req}

-- call_event HANGUP -->
                       -- hangup --------->
                       <- ACK member_call --
                                         {ready}
                    {unbind}

                       ----------------------------- member_connect_req ->
                       <-- member_connect_resps --------------------------
                       -- member_connect_resps ->
                       <- send_conn_win --
                                       {member_conning}

-- call_event HANGUP -->
                       -- hangup --------->
                       <- ACK member_call --
                                         {ready}
                    {unbind}
```

This diagram shows how the FSM reacts to receiving a HANGUP either during:
* **member_conn_req**: ignores `member_connect_resp`s, unbinds from the call events, and ACKs the `member_call` so the queue can progress
* **member_conning**: ignores `member_connect_accepted`/`retry` messages, unbinds from the call events, and ACKs the `member_call`

#### Supervisors

A supervisor is able to listen/coach an agent while on the phone with a customer.

> Supervisor process layout

Similar to agent processes. The FSM will manage the lifecycle of a coach/whisper/listen

```asciiart
 [acdc_supers_sup]
          |
          |----[acdc_super_sup]
          |----[acdc_super_sup]
                       |
                       |----acdc_super_listener
                       |----acdc_super_fsm
```

##### Supervisor startup
