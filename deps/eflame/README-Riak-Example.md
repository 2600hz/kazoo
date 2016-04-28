
# A detailed example of investigations using flame graphs and Riak

In our very first example, as described in
[README.md](README.md), we generated a flame graph for all Riak
processes that were executing during:

* A time window of 10 seconds
* We ran 25 Riak `get` requests while inside the measurement time window

... and we got something that looks like this:

<a href="http://www.snookles.com/scotttmp/eflame2/riak.0.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.0.png"></a>

Wow, that's impossible to read without zooming in via the SVG version
of the image.  (Remember that the SVG versions of flame graphs are
"zoomable".  Click on the PNG image of any flame graph in this
document to fetch the SVG version of that image.)

## Strip out Erlang process sleeping/descheduled time

However, we know that the flame graphs are colored with "SLEEP" time
as a purple bar at the top of the stack.  We can use this command to
strip out all Erlang process sleep time and create a new graph:

    cat /tmp/ef.test.0.out | grep -v 'SLEEP ' | \
        ./flamegraph.riak-color.pl > /tmp/output.0.nosleep.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.1.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.1.png"></a>

Now we can see a few PIDs near the bottom: `0.1333.0` and `0.28843.0`
and so on.  But this is still too cluttered: there are over 150 Erlang
processes in this graph, with stacks for each process presented
**separately**.

## Collapse stacks for different processes

What happens if we both:

* ignore sleep time, and
* collapse same stacks in different processes?

Let's try it.

    cat /tmp/ef.test.0.out | grep -v 'SLEEP ' | sed 's/^[^;]*;//' | \
        ./flamegraph.riak-color.pl > /tmp/output.0.nosleep-nopid.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.2.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.2.png"></a>

Now we can see several sets of related activities!  Going from left to
right:

1. (All in orange) Miscellaneous OTP processes
2. (With green, on top of `gen_fsm:handle_msg/7`) Riak KV get FSM
   process activity intermixed with Riak Core vnode activity.
   The Riak vnode also uses the `gen_fsm` behavior, so the stacks for
   these very dissimilar-acting processes are collapsed to the same
   base of `gen_fsm:handle_msg/7` near the bottom of their call stacks.
3. (With green, on top of `gen_server:handle_msg/5`) Riak Core and KV
   vnode process activity.
4. (On top of `riak_core_stat_cache:'-do_get_stats/2-fun-0-'/4`) Riak
   Core and KV statistics processing (both for the get FSM activity and
   also for OS stats monitoring).
5. (On top of `shell:eval_loop/3`) Activity related to the Riak
   console and the Erlang REPL shell.

## Look at a single process

Let's look at a single process.  We are interested in Riak KV vnode
processes that are executing Bitcask code.  So, if we use
`grep bitcask /tmp/ef.test.0.out`, we see that PID `0.450.0` has a lot
of activity.  Let's see what that PID alone is doing.

    cat /tmp/ef.test.0.out | egrep '0\.450\.0' | \
        ./flamegraph.riak-color.pl > /tmp/output.0.only-450.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.3.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.3.png"></a>

Oops, that process is sleeping, mostly.  Let's ignore the sleep time
also.

    cat /tmp/ef.test.0.out | egrep '0\.450\.0' | grep -v 'SLEEP ' | \
        ./flamegraph.riak-color.pl > /tmp/output.0.only-450-nosleep.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.4.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.4.png"></a>

Now we have a nice breakdown for what a Riak KV vnode process is doing
while under our workload.  Our workload only performs get operations,
so it should be no surprise that we do not see any put or delete
activity.

Also, during the 10 seconds that we were profiling, this process was
asked to check its backend status.  Since this Riak cluster is using
Bitcask, we see the Bitcask-related activity for handling the status
request(s).

## Let's make a code change and see how the graphs change.

Let's assume that we've done some work, altering the behavior of the
Riak system's get operation.  We have altered both the client side
(i.e., the get FSM process) and also the server side (the vnode
process).  Performance of the new code is terrible.  Why?

Let's try to find out.  We cut-and-paste this single blob into the
Riak console shell:

   spawn(fun() ->
       io:format("Tracing started...\n"),
       eflame2:write_trace(like_fprof, "/tmp/ef.test.1", all, timer, sleep, [10*1000]),
       io:format("Tracing finished!\n")
    end).
    
    timer:sleep(1000).
    f(C).
    {ok, C} = riak:local_client().
    [C:get(<<"test bucket">>, <<Key:32>>) || Key <- lists:seq(1, 25)].

Then process the trace:

    eflame2:format_trace("/tmp/ef.test.1", "/tmp/ef.test.1.out").
    %% ... wait for the trace to finish generating the text output ...

Then we create the SVGs by using a couple of extra flags for the Perl
script: `--hash` and `--seed`.  This forces the color choices to be
the same for functions with exactly the same name ... the sameness
makes it easier to see differences in two graphs.

    cat /tmp/ef.test.0.out | egrep '0\.450\.0' | grep -v 'SLEEP ' | \
        ./flamegraph.riak-color.pl --seed 42 --hash > /tmp/output.0.only-450-nosleep.svg
    cat /tmp/ef.test.1.out | egrep '0\.450\.0' | grep -v 'SLEEP ' | \
        ./flamegraph.riak-color.pl --seed 42 --hash > /tmp/output.1.only-450-nosleep.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.10.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.10.png"></a>
<br>
<a href="http://www.snookles.com/scotttmp/eflame2/riak.11.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.11.png"></a>

Oops, our profiling the first time (in the `/tmp/ef.test.0.out` file)
caught some `riak_kv_vnode:handle_command/3` activity that the second
version does not.  Let's remove those call stack entries and compare
again.

    cat /tmp/ef.test.0.out | egrep '0\.450\.0' | grep -v riak_kv_vnode:handle_command/3 | grep -v 'SLEEP ' | \
        ./flamegraph.riak-color.pl --seed 42 --hash > /tmp/output.0.only-450-nosleep.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.12.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.12.png"></a>
<br>
<a href="http://www.snookles.com/scotttmp/eflame2/riak.11.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.11.png"></a>

Hrrrmmmm, those graphs are pretty similar.  Very similar.  Perhaps
within the normal & typical range of individual runs.  We could try to
continue by:

1. Performing 10 or more runs of each type, before & after our patch.
   Then see how much variation there is in each graph.
2. Look at other parts of the system that also changed with our patch.
   Perhaps the real problem is not in the vnode code at all!
3. Let's try one more quick experiment: try looking at the flame
   graphs, before & after the patch, but **include** the sleep time.

The commands to generate the SVGs:

    cat /tmp/ef.test.0.out | egrep '0\.450\.0' | grep -v riak_kv_vnode:handle_command/3 | \
        ./flamegraph.riak-color.pl --seed 42 --hash > /tmp/output.0.only-450.svg
    cat /tmp/ef.test.1.out | egrep '0\.450\.0' | \
        ./flamegraph.riak-color.pl --seed 42 --hash > /tmp/output.1.only-450.svg

<a href="http://www.snookles.com/scotttmp/eflame2/riak.13.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.13.png"></a>
<br>
<a href="http://www.snookles.com/scotttmp/eflame2/riak.14.svg"><img src="http://www.snookles.com/scotttmp/eflame2/riak.14.png"></a>

**YES!** There's a big difference now.  After our patch, the bottom
half clearly shows that there's an extra function that is forcing the
vnode to go to sleep during `riak_core_vnode:vnode_command/3` work!

What is that change that we made?  This is an artificial example, but
here's the main reason for the difference: a call to a new function,
`riak_kv_vnode:do_alternate_processing/1`, included a 'receive' clause
that waits for some optional data.

    diff --git a/src/riak_kv_vnode.erl b/src/riak_kv_vnode.erl
    index 80e2a50..6d7b287 100644
    --- a/src/riak_kv_vnode.erl
    +++ b/src/riak_kv_vnode.erl
    @@ -1913,6 +1914,14 @@ uses_r_object(Mod, ModState, Bucket) ->
         {ok, Capabilities} = Mod:capabilities(Bucket, ModState),
         lists:member(uses_r_object, Capabilities).
     
    +do_alternate_processing(S) ->
    +    receive
    +        {extra_mod_state, Stuff} ->
    +            S#state{modstate=Stuff}
    +    after 20 ->
    +        S
    +    end.
    +
     -ifdef(TEST).
     
     %% Check assigning a vnodeid twice in the same second

The `after 20` clause is the reason why the process is put to sleep by
the VM's scheduler.  And the flame graph makes this extra sleep very
obvious to the programmer's eye.
