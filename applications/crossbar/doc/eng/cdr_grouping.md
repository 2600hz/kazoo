/*
Section: Crossbar Engineering
Title: CDR Grouping
Language: en-US
Version: 3.21
*/

The problem: each leg of a call is independently tracked, which makes it challenging to know which legs are associated with a particular call.

The solution:

We looked at four scenarios initially:

1. Single leg calls (such as to an IVR or voicemail)
1. Two legs (typical Alice calls Bob scenarios).
1. Blind transfers with three legs
1. Attended transfers with four legs

Each CDR tracks both the `call_id` and `other_leg_call_id` (if applicable). So scenarios 1 and 2 are trivial to group.

## The basic scenarios

Let's call scenario 1 leg A and scenario 2 has legs B and C. If we take the legs indentifed in the CDRs as keys, and the call\_id of the CDR as a value, we can create a list of values for a given call\_id. This would give us:

    Intermediate Map
    {"A":["A"]
     ,"B":["B", "C"]
     ,"C":["C", "B"]
    }

This assumes we sort the legs by their timestamp (more on that later). We then apply the following pseudo-code to get the CDRs grouped according to their calls:

    foreach array in the intermediate map:
        sort the array by timestamp
        take the head (first element) of the array as the initiating leg
        merge the sorted array into a map using the head as the key


What might this look like given our object above?

1. First Iteration
    * Array = `["A"]`, FinalMap = `{}`
        * SortedArray = `["A"]`
        * ALeg = `hd(SortedArray)` = `"A"`
        * Existing = `get(ALeg, FinalMap)` = `[]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"A":["A"]}`
2. Second Iteration
    * Array = `["B", "C"]`, FinalMap = `{"A":["A"]}`
        * SortedArray = `["B", "C"]`
        * ALeg = `hd(SortedArray)` = `"B"`
        * Existing = `get(ALeg, FinalMap)` = `[]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"A":["A"], "B":["B", "C"]}`
3. Third Iteration
    * Array = `["C", "B"]`, FinalMap = `{"A":["A"], "B":["B", "C"]}`
        * SortedArray = `["B", "C"]`
        * ALeg = `hd(SortedArray)` = `"B"`
        * Existing = `get(ALeg, FinalMap)` = `["B", "C"]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"A":["A"], "B":["B", "C"]}`

So the resulting map would have the a-leg call\_id as the top-level keys and the values would be the sorted list of CDRs involved in the call.

Now, if these two scenarios were all we ever had to deal with, this would be quite easy to build in a view in the database (since we have all the information we need in a CDR for which legs are involved.

Unfortunately, Kazoo supports transfers! These scenarios involve multiple legs and must be handled differently.

## Blind Transfers

Our intermediate map will look differently when a blind transfer occurs, since we'll have three legs to group:

The call scenario:

* "D" calls "E"
* "E" blind transfers "D" to "F"
* "F" talks to "D"

The resulting intermediate map would be:

    {"D":["D", "F"]
     ,"E":["E", "D"]
     ,"F":["F", "D"]
    }

Let's apply our function over this map:

1. First Iteration
    * Array = `["D", "F"]`, FinalMap = `{}`
        * SortedArray = `["D", "F"]`
        * ALeg = `hd(SortedArray)` = `"D"`
        * Existing = `get(ALeg, FinalMap)` = `[]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"D":["D", "F"]}`
2. Second Iteration
    * Array = `["E", "D"]`, FinalMap = `{"D":["D", "F"]}`
        * SortedArray = `["D", "E"]`
        * ALeg = `hd(SortedArray)` = `"D"`
        * Existing = `get(ALeg, FinalMap)` = `["D", "F"]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"D":["D", "E", "F"]}`
3. Third Iteration
    * Array = `["F", "D"]`, FinalMap = `{"D":["D", "E", "F"]}`
        * SortedArray = `["D", "F"]`
        * ALeg = `hd(SortedArray)` = `"D"`
        * Existing = `get(ALeg, FinalMap)` = `["D", "E", "F"]`
        * set(ALeg, merge(Existing, SortedArray), FinalMap)
        * FinalMap = `{"D":["D", "E", "F"]}`

We can see we've identified `"D"` as the a-leg and include the "D", "E", and "F" CDRs in the call.

## Attended transfers

Anyone working in VoIP will tell you attended transfers are a pain; fortunately processing the associated CDRs is no different!

First, the call scenario:

* G calls H
* H puts G on hold and, independently, starts I for J
* I and J talk
* I hangs up, which hangs up H as well
* G and J talk

Got that?

The intermediate map then becomes:

    {"G", ["G", "H", "J"]
     ,"H", ["H"]
     ,"I", ["I"]
     ,"J", ["G", "I", "J"]
    }

We can see that merging `"G"` and `"J"` is going to give us all four legs, but what do with `"H"` and `"I"`? As it stands, the resulting map would include those are single-leg calls. Well, remember what we said about the each CDR having the call\_id **and** the other leg's call\_id (if any)? The "(if any)" part turns out to be particularly helpful here. True single-leg calls won't have one, while `"H"` and `"I"` will have other leg call\_ids. So we filter out and single-leg arrays where the CDR of the leg has another leg associated with it.

The attended transfer's final map would then be:

    {"G":["G", "H", "I", "J"]}

If we were to process all four scenarios, the response would be:

    {"A":["A"]
     ,"B":["B", "C"]
     ,"D":["D", "E", "F"]
     ,"G":["G", "H", "I", "J"]
    }

Huzzah, life is good.

See the `group_cdrs/1` in `cb_cdrs.erl` for the entry into the process (`group` builds the intermediate map, `combine` builds the final map).

## A note about timestamps

The timestamp on the CDR will be when the call terminated (thus generating the CDR). Since most two-party legs end at the same time when hanging up, we need to differentiate them a bit more to determine the a-leg.

First, we look at the call\_direction and decrement by 1 if it is `inbound` as this is the most common way a call is initiated. We then also decrement by the `duration_seconds` since the a-leg will typically have a larger duration (or at least equal) to the b-leg.

So `SortingTimestamp = Timestamp - call_direction_adjustment(CDR) - duration_seconds(CDR)`.
