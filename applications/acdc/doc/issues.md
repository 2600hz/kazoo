### Known Issues

#### Round Robin strategy

In a multi node/zone cluster round robin doesn't work as an enduser would expect.

The problem is that each node in the cluster has an independent Queue Manager process which picks the next suitable agent. Queue Managers only manage the node and so calls handled by other nodes in the cluster are not taken into account when selecting the next agent.

Consider this scenario:

Lets say we have a round robin queue Q1 with 4 agents (A1, A2, A3, A4) all with the same priority and there are 2 nodes in a cluster N1 and N2.

when we start up Q1 will have a Queue Manager process on N1 and a Queue Manager process on N2. Each manager creates its own agent queue AQ1 and AQ2. Lets assume the Agents are initally added to the AQs in the same order [A1,A2,A3,A4]

Now a call comes into Q1 and is handled by N1. The Queue Manager on N1 looks at the head of AQ1 and selects agent A1 for taking the call, A1 is then put at the back so AQ1 on N1 becomes [A2,A3,A4,A1] and agent A1's device starts ringing.

A1 finishes the call.

A 2nd call comes in and this time is handled by N2. AQ2 on N2 is still [A1,A2,A3,A4] and so A1 is selected again, hardly round robin.

If, however, the 2nd call was again handled by N1 then everything would be as expected as A2 is at the head of AQ1.

So the problem is how to tell N2 to move A1 to the back so that it matches N1. A possible solution would be to broadcast a federated agent_win message on the AMQP bus that all Queue Managers listen to and they update thier AQ. Another possible solution would be to make the AQ a shared resource by implementing it as a AMQP worker queue, but this would be a much bigger task. 
