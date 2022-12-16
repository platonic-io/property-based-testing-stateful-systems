Simulation testing
==================

![](../images/under_construction.gif)

*While the overall structure and code will likely stay, there’s still more work
 needed to turn this part from a bullet point presentation into a readable
 text.*

Motivation
----------

For many systems the [combination](./Part03SMContractTesting.md#readme) of
integration tests and (consumer-driven) contract tests together with [smoke
tests](https://en.wikipedia.org/wiki/Smoke_testing_(software)), and potentially
some [fault injection](./Part04FaultInjection.md#readme), should yield a
test-suite with "good enough" coverage.

Fault tolerant distributed systems, such as for example distributed databases,
are perhaps an exception to the above rule of thumb. The reason for this is that
networking faults (e.g. partitions or packet loss), which these systems are
supposed to be tolerant against, are difficult to inject even if we have fakes
for all dependencies and follow the steps we did
[previously](./Part04FaultInjection.md#readme).

In a sense this is witnessed by the effectiveness of
[Jepsen](https://github.com/jepsen-io/jepsen#jepsen) when testing distributed
databases, where injecting network faults plays a big part. Given Jepsen's
[success](https://jepsen.io/analyses) of finding bugs in pretty much every
distributed database it has been pointed at, it's interesting to ask: which
distributed databases passed the Jepsen test and what kind of testing did they
do in order to do so?

The perhaps most famous example is
[FoundationDB](https://www.foundationdb.org/). When Kyle "aphyr" Kingsbury was
asked if he was planning on Jepsen testing FoundationDB soon, he
[replied](https://twitter.com/aphyr/status/405017101804396546):

 > "haven't tested foundation[db] in part because their testing appears to be
 > waaaay more rigorous than mine."

So in this part we'll have a look at the technique that FoundationDB is tested
with -- simulation testing.

Plan
----

We've already noted that injecting network faults is difficult in the setup we
saw previously. Another problem is: even if we could somehow inject a fault that
caused, say, a message to be dropped, we'd still need to wait for the associated
timeout and send retry. Retry timeouts can sometimes take several seconds, so if
we have many network faults our tests would get very slow (Jepsen has this
problem as well).

So here's an idea: what if we treated networking and time as "dependencies", and
put them behind interfaces and implement a fake for them similar to how we did
in [part 3](./Part03SMContractTesting.md#readme) and [part
4](./Part04FaultInjection.md#readme)?

This would allow us to deterministically _simulate_ a network of components by
connecting the fakes and let them "communicate" with each other. The fake time
could be advanced discretely based on when messages arrive rather with a real
clock, that way we don't have to wait for timeouts to happen.

While the software/system under test (SUT) is running in our simulator we hit it
with client requests and collect a concurrent history, after the simulation is
done we check if the history linearises (like in [part
2](./Part02ConcurrentSMTesting.md#readme)). Other global assertions on the state
of the whole system are also possible.

To make debugging easier we'll also write a time-travelling debugger that lets
us step through the history of messages and view how the state of each state
machine evolves over time. The reason this works is because our system is
deterministic, so we can record and replay the inputs of the system to recreate
the system state at any point in time.

A, perhaps, useful analogy here is that imagine if we are building an airplane,
then we might do testing in a wind-tunnel. The wind-tunnel lets us speed up
testing, e.g. we don't have to sit around and wait for an actual hurricane to
happen for us to test how the plane behaves in such a situation. Likewise our
simulator can create worst case scenarios that would take years of real-world
traffic to occur. When something bad happens we can inspect the airplane's
blackbox after the fact in order to reconstruct what might have caused the
failure, this is what our debugger is supposed to be able to do.

How it works
------------

Imagine we want to write an distributed in-memory data store, where client
requests can either write or read some data.

For each client write there will be several internal messages between the nodes
that make up the distributed network to ensure that enough nodes got a copy of
the data, before a client response is sent in order to ensure that the data is
"durable" (i.e. can be replicated in case some nodes crash). The picture looks
something like this:

  ![](../images/part5-data-store.svg){ width=500px }

How can this be achieved? One way would be for the distributed network to elect
a leader node and have all client requests go through it, the leader would then
replicate the data to all other nodes and confirm enough nodes got it before
responding to the client. In case the leader because unavailable, a new leader
is elected. In case a node crashes, its state is restored after it restarts by
the other nodes. That way as long as enough nodes are available and running we
can keep serving client requests. We'll omit the exact details of how this is
achieved or now, but hopefuly we've explained enough for it to be possilbe to
appreciate that testing all possible corner cases related to those failure modes
can be tricky.

Next lets sketch how we can implement the data store nodes using state machines
(SMs). First recall the type of our SMs:

```haskell
    Input -> State -> (State, Output)
```

Client requests and internal messages from other nodes in the network can be
modelled as inputs, and client responses as well as outgoing messages to other
nodes are the outputs, while the data store is part of the state.

Note that if implemented this way the "business logic" of the data store is
completely separated away from actually sending and receiving requests and
messages over the network. This is key for being able to simulate the network,
as we shall see later.

How does the actual networking happen though? For the "real" / "production"
deployment of the data store we start a HTTP server, where concurrent requests
and messages can be sent to the data store, these network events get written to
a FIFO queue which the SM processes one by one:

  ![](../images/part5-real-event-loop.svg){ width=600px }

Sometimes when we send internal messages to other nodes they can be dropped by
the network, in order to be able to implement retry logic we need to extend the
basic functionality of SMs with some notion of being able to keep track of the
passage of time. There are many ways to do this, for our particular application
we'll choose timers. The way timers work is that SMs can register a timer as
part of their output. Typically we'd do something like: send such and such
message to such and such node and set a timer for 30s, if we don't hear back
from the node within 30s and reset the timer, then a timer wheel process will
enqueue a timer event which the SM can use for doing the retry.

By the way, all this extra stuff that happens outside of the SM is packaged up
in a componenet called the event loop.

Before we deploy the SM to production using the above event loop, we would like
to test it for all those tricky failure modes we mentioned before. In order to
reuse as much code as possible with the "real" / "production" deployment, we'll
use the same SM and event loop!

How can we possibly reuse the same event loop you might be thinking? The key
here is that the networking and timer wheel components of the event loop are
implemeneted using interfaces.

The interface for networking has a method for sending internal messages to other
nodes and a method for sending responses to clients (note that we don't need a
method for receiving because that's already done by the event loop and we can
merely dequeue from the event queue to get the network events). The interface
for time has a method to get the current time as well as setting the current
time.

Interfaces can have several implementations which we can "inject" at the start
of the event loop. Hopefully the implementation for the "real" deployment should
be clear enough (it's just what you'd expect using the real network and real
clock). The "fake" / simulation deployment of the event loop has a more
interesting implementation though, this is what it looks like:

  ![](../images/part5-simulation-event-loop.svg){ width=600px }

The first thing to notice is that in simulation we won't have actual clients
making requests, instead there's a client generator component which will
generate those on the fly instead.

We didn't say this earlier, but the event queue is also an interface (with
enqueue and dequeue methods) and during simulation we replace the FIFO queue
with a priority queue sorted by event arrival time.

So the client generator generates not just a request but also an arrival time of
that request and then enqueues the network event. It also appends it to the
concurrent history, which will later be used for checking and debugging.

In the "real" / "production" deployment we run one SM, which encodes the
"business logic" of a data store, on top of a event loop per node in the
network. In the simulation deployment we deploy all the SMs of the nodes on top
of the same event loop, i.e. the whole distributed network will be run
"locally", so the event loop needs to be able to dequeue events and hand them to
the appropriate SM.

Once the SM is stepped and we get its outputs, the "fake" send implementation of
the network interface will generate arrival times and put them back on the
priority queue, while the "fake" respond implementation will notify the client
generator and append the response to the concurrent history.

Note that since arrival times are randomly generated (deterministically using a
seed) and because we got a priority queue rather than a FIFO we get interesting
message interleavings where for example a message that was sent much later than
some other message might end up getting receieved earlier.

Next lets have a look at time. The "fake" implementation of the time interface
is completely detached from the actual system time, the clock is only advanced
by explicit calls to the set time method. This allows us to do a key thing: set
the time when we dequeue an event to the arrival time of that event! This allows
us to jump in time to when we know that the next event is supposed to happen
without waiting for it, i.e. no more waiting 30s for timeouts to happen!

Speaking of timeouts, lets have a look at how we can inject network faults which
will eventually trigger "unhappy" code paths that eventually trigger timeouts.
If we wanted to add a fault which randomly drops some messages we could
implement yet another variant of the network interface where the send method
simply doesn't enqueue some of the messages (effectively dropping them). We
could similarly introduce high latency for some messages by generating arrival
times with a greater variance.

The simulation stops running when when we reach some predefined end time. At
this point we got a concurrent history which contains both client requests and
responses (i.e. the black-box traffic) as well as all internal messages between
the nodes (i.e. the white-box traffic).

Using the black-box history we can apply linearisability checking to test à la
Jepsen, but faster and deterministically. While the white-box history can be used
for assertions about the global state of the nodes in the network, e.g. check
that no two nodes disagree about the content of the data store. We can also
build a debugger that can step through the history and show how each SM evolves
over time, making debugging more convenient.

If the checkers find any problem, we want to be able to reproduce it from a
single seed which the simulation event loop uses to generate arrival times, if
messages should be dropped or not, etc. It's key that the simulation event loop
is deterministic otherwise we can't do that.

Code
----

 <!---

> module Part05SimulationTesting () where

-->

* Let's start with the state machine (SM) type
* A bit more complex what we've seen previously
  - input and output types are parameters so that applications with different message types can written
  - inputs are split into client requests (synchronous) and internal messages (asynchrous)
  - a step in the SM can returns several outputs, this is useful for broadcasting
  - outputs can also set and reset timers, which is necessary for implementing retry logic
  - when the timers expire the event loop will call the SM's timeout handler (`smTimeout`)
  - in addition to the state we also thread through a seed, `StdGen`, so that the SM can generate random numbers
  - there's also an initisation step (`smInit`) to set up the SM before it's put to work

> import Part05.StateMachine ()

* In order to make it more ergonomic to write SMs we introduce a domain-specific
  language (DSL) for it

* The DSL allows us to use do syntax, do `send`s or register timers anywhere
  rather than return a list outputs, as well as add pre-conditions via guards
  and do early returns

> import Part05.StateMachineDSL ()

* The SMs are, as mentioned previously, parametrised by their input and output
  messages.

* These parameters will most likely be instantiated with concrete (record-like) datatypes.

* Network traffic from clients and other nodes in the network will come in as
  bytes though, so we need a way to decode inputs from bytes and a way to encode
  outputs as bytes.

* `Codec`s are used to specify these convertions:

> import Part05.Codec ()

* A SM together with its codec constitutes an application and it's what's expected from the user
* Several SM and codec pairs together form a `Configuration`
* The event loop expects a configuration at start up

> import Part05.Configuration ()

* We've covered what the user needs to provide in order to run an application on
  top of the event loop, next lets have a look at what the event loop provides

* There are three types of events, network inputs (from client requests or from
  other nodes in the network), timer events (triggered when timers expire), and
  commands (think of this as admin commands that are sent directly to the event
  loop, currently there's only a exit command which makes the event loop stop
  running)

> import Part05.Event ()

* How are these events created? Depends on how the event loop is deployed: in
  production or simulation mode

> import Part05.Deployment ()

* network interface specifies how to send replies, and respond to clients

* Network events in a production deployment are created when requests come in on http server
  - Client request use POST
  - Internal messages use PUT

  - since client requests are synchronous, the http server puts the client
    request on the event queue and waits for the single threaded worker to
    create a response to the client request...

* network events in a simulation deployment are created by the simulation itself, rather than from external requests
  - Agenda = priority queue of events
  - network interface:
    ```
     { nSend    :: NodeId -> NodeId -> ByteString -> IO ()
     , nRespond :: ClientId -> ByteString -> IO () }
    ```

> import Part05.Network ()
> import Part05.AwaitingClients ()
> import Part05.Agenda ()

* Timers are registerd by the state machines, and when they expire the event loop creates a timer event for the SM that created it
* This is the same for both production and simulation deployments
* The difference is that in production a real clock is used to check if the
  timer has expired, while in simulation time is advanced discretely when an
  event is popped from the event queue

> import Part05.TimerWheel ()

* These events get queued up, and thus an order established, by the event loop
  - XXX: production
  - XXX: simulation
  - interface:
  ```
  data EventQueue = EventQueue
    { eqEnqueue :: Event -> IO ()
    , eqDequeue :: DequeueTimeout -> IO Event
    }
  ```

> import Part05.EventQueue ()

* Now we have all bits to implement the event loop itself

> import Part05.EventLoop ()

* Last bits needed for simulation testing: generate traffic, collect concurrent
  history, debug errors:

> import Part05.ClientGenerator ()
> import Part05.History ()
> import Part05.Debug ()

* Finally lets put all this together and develop and simulation test
  [Viewstamped replication](https://dspace.mit.edu/handle/1721.1/71763) by Brian
  Oki, Barbra Liskov and James Cowling (2012)

XXX: Viewstamp replication example...

Discussion
----------

- Q: This is a lot of code that's unrelated to the application that I want to
     write, is it worth the effort?

  A: Ideally much of it can be packaged up and reused among applications. But
     even if you end up having to write everything yourself, perhaps because
     nobody else has yet done it in your programming language of choice, it's
     probably worth it if you want to pass the Jepsen test (and more
     importantly: that you keep passing it as you change your code).

     Joran Dirk Greef gave a [talk](https://www.youtube.com/watch?v=FyGukn77gqA)
     at the CMU database [seminar](https://db.cs.cmu.edu/seminar2022/) about the
     TigerBeetle database where he said that having a simulator was one of his
     favorit aspects of the database and accredited increased developer velocity
     to it.

- Q: Writing the application on state machine form, even with the DSL, seems
     restrictive?

  A: Yes, it's only by enforcing this structure on the application that we are
     able to exploit it later in the testing phase. For comparison, FoundationDB
     had to implement their own programming
     [language](https://apple.github.io/foundationdb/flow.html) to achieve this.

- Q: What are the risks of simulation testing being wrong somehow?

  A: We've made an assumption: each message is processed atomically by each
     state machine. This assumption is reasonable as long as the state machines
     don't share state. A more fine-grained approach where each state machine
     has a "program counter" that gets incremented is imaginable, but will
     introduce a lot more complexity and many further states.

     Even though we tried to minimise difference between "production" and
     simulation testing deployment there's always going to be a gap between the
     two where bugs might sneak in, for example there could be something wrong
     in the implementation of the real network interface.

     Another possilbe gap is that the faults we inject aren't realistic or
     complete. A good source for inspiration for faults is Deutsch's [fallacies
     of distributed
     computing](https://en.wikipedia.org/wiki/Fallacies_of_distributed_computing).
     Jepsen's list of
     [nemesis](https://github.com/jepsen-io/jepsen/blob/e7446a44c06bdc7996f989d1e8c39624c697c82a/jepsen/src/jepsen/nemesis/combined.clj#L507),
     the Chaos engineering communties
     [faults](https://medium.com/the-cloud-architect/chaos-engineering-part-3-61579e41edd8)
     and FoundationDB's simulator's
     [faults](https://apple.github.io/foundationdb/testing.html) are other good
     sources.

     The FoundationDB CTO was apparently worried about the simulator
     subconsciously training their programmers to beat it, see relevant part of
     Will Wilson's [talk](https://youtu.be/4fFDFbi3toc?t=2164) for more on this
     topic.

     Finally, even if we are reasonably sure that we've mitigated all the above
     risks, we will be facing the state space explosion problem. There's simply
     too many ways messages can get interleaved, dropped, etc for us to be able
     to cover all during testing. The only way around the state explosion
     problem is to use proof by induction. A simple analogy: we cannot test that
     some property holds for all natural numbers, but we can in merely two steps
     prove by induction that it does. For more on this topic see Martin
     Kleppmann's
     [work](https://lawrencecpaulson.github.io/2022/10/12/verifying-distributed-systems-isabelle.html).

- Q: Where do these ideas come from?

  A: The first reference we been able to find is the following concluding remark
     by Alan Perlis (the first recipient of the Turing Award) about a discussion
     about a paper on simulation testing by Brian Randell at the first
     [conference](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF)
     on Software Engineering (this is where we got the term from) in 1968:

   >  "I’d like to read three sentences to close this issue.
   >
   >   1. A software system can best be designed if the testing is interlaced with
   >      the designing instead of being used after the design.
   >
   >   2. A simulation which matches the requirements contains the control which
   >      organizes the design of the system.
   >
   >   3. Through successive repetitions of this process of interlaced testing and
   >      design the model ultimately becomes the software system itself. I think that it
   >      is the key of the approach that has been suggested, that there is no such
   >      question as testing things after the fact with simulation models, but that in
   >      effect the testing and the replacement of simulations with modules that are
   >      deeper and more detailed goes on with the simulation model controlling, as it
   >      were, the place and order in which these things are done."

     The idea in its current shape and as applied to distributed systems was
     introduced(?), or at the very least popularised, by Will Wilson's talk
     [*Testing Distributed Systems w/ Deterministic
     Simulation*](https://www.youtube.com/watch?v=4fFDFbi3toc) at Strange Loop (2014)
     about how they used [simulation
     testing](https://apple.github.io/foundationdb/testing.html) to test
     [FoundationDB](https://www.foundationdb.org/) (so well that Kyle "aphyr"
     Kingsbury didn't feel it was
     [worth](https://twitter.com/aphyr/status/405017101804396546) Jepsen testing
     it) as mentioned in the motivation section.

     Watching the talk and rereading the Perlis quote makes one wonder: was the
     technique independently rediscovered, or had they in fact read the
     (in)famous 1968 NATO software engineering report?

     There's also the more established practice of [discrete-event
     simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation) which
     is usually used in different contexts than software testing, but
     nevertheless is close enough in principle that it's worth taking
     inspiration from (and indeed the simulation testing people often refer to
     it).

     [John Carmack](https://en.wikipedia.org/wiki/John_Carmack) wrote an
     interesting
     [.plan](https://raw.githubusercontent.com/ESWAT/john-carmack-plan-archive/master/by_day/johnc_plan_19981014.txt)
     about recoding and replaying events in the context of testing in 1998, and
     other
     [developers](http://ithare.com/testing-my-personal-take-on-testing-including-unit-testing-and-atddbdd/)
     in the the game industry are also advocating this technique.

     Three Amazon Web Services (AWS) engineers recently published a paper called
     [Millions of Tiny
     Databases](https://www.usenix.org/conference/nsdi20/presentation/brooker) (2020)
     where they say:

   > "To solve this problem [testing distributed systems], we picked an approach that
   > is in wide use at Amazon Web Services, which we would like to see broadly
   > adopted: build a test harness which abstracts networking, performance, and
   > other systems concepts (we call it a simworld). The goal of this approach is to
   > allow developers to write distributed systems tests, including tests that
   > simulate packet loss, server failures, corruption, and other failure cases, as
   > unit tests in the same language as the system itself. In this case, these unit
   > tests run inside the developer’s IDE (or with junit at build time), with no need
   > for test clusters or other infrastructure. A typical test which tests
   > correctness under packet loss can be implemented in less than 10 lines of Java
   > code, and executes in less than 100ms. The Physalia team have written hundreds
   > of such tests, far exceeding the coverage that would be practical in any
   > cluster-based or container-based approach.
   >
   > The key to building a simworld is to build code against abstract physical layers
   > (such as networks, clocks, and disks). In Java we simply wrap these thin layers
   > in interfaces. In production, the code runs against implementations that use
   > real TCP/IP, DNS and other infrastructure. In the simworld, the implementations
   > are based on in-memory implementa- tions that can be trivially created and torn
   > down. In turn, these in-memory implementations include rich fault-injection
   > APIs, which allow test implementors to specify simple statements like:
   > `net.partitionOff ( PARTITION_NAME , p5.getLocalAddress () ); ...
   > net.healPartition ( PARTITION_NAME );`
   >
   > Our implementation allows control down to the packet level, allowing testers
   > to delay, duplicate or drop packets based on matching criteria. Similar
   > capabilities are available to test disk IO. Perhaps the most important testing
   > capability in a distributed database is time, where the framework allows each
   > actor to have it’s own view of time arbitrarily controlled by the test.
   > Simworld tests can even add Byzantine conditions like data corruption, and
   > operational properties like high la- tency. We highly recommend this testing
   > approach, and have continued to use it for new systems we build."

     [Dropbox](https://en.wikipedia.org/wiki/Dropbox) has written
     [several](https://dropbox.tech/infrastructure/rewriting-the-heart-of-our-sync-engine)
     [blog](https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine)
     [posts](https://dropbox.tech/infrastructure/-testing-our-new-sync-engine)
     related to simulation testing.

     Basho's [Riak](https://en.wikipedia.org/wiki/Riak) (a distributed NoSQL
     key-value data store that offers high availability, fault tolerance,
     operational simplicity, and scalability) also uses similar
     [techniques](https://speakerdeck.com/jtuple/hansei-property-based-development-of-concurrent-systems)
     for their testing.

     Finally, [IOG](https://iog.io/) published a
     [paper](http://www.cse.chalmers.se/~rjmh/tfp/proceedings/TFP_2020_paper_11.pdf)
     called "Flexibility with Formality: Practical Experience with Agile Formal
     Methods in Large-Scale Functional Programming" (2020), where they write:

   > "Both the network and consensus layers must make significant use of
   > concurrency which is notoriously hard to get right and to test. We
   > use Software Transactional Memory (STM) to manage the internal state
   > of a node. While STM makes it much easier to write correct concurrent
   > code, it is of course still possible to get wrong, which leads to
   > intermittent failures that are hard to reproduce and debug.
   >
   > In order to reliably test our code for such concurrency bugs,
   > we wrote a simulator that can execute the concurrent code with
   > both timing determinism and giving global observability, producing
   > execution traces. This enables us to write property tests that can
   > use the execution traces and to run the tests in a deterministic
   > way so that any failures are always reproducible.  The use of the
   > mini-protocol design pattern, the encoding of protocol interactions
   > in session types and the use of a timing reproducable simulation has
   > yielded several advantages:
   >
   >   * Adding new protocols (for new functionality) with strong
   >     assurance that they will not interact adversly with existing
   >     functionality and/or performance consistency.
   >
   >   * Consistent approaches (re-usable design approaches) to issues
   >     of latency hiding, intra mini-protocol flow control and
   >     timeouts / progress criteria.
   >
   >   * Performance consistent protocol layer abstraction /
   >     subsitution: construct real world realistic timing for operation
   >     without complexity of simulating all the underlying layer protocol
   >     complexity. This helps designs / development to maintain performance
   >     target awareness during development.
   >
   >   * Consitent error propagation and mitigation (mini protocols to
   >     a peer live/die together) removing issues of resource lifetime
   >     management away from mini-protocol designers / implementors."

    The simulation code is open source and can be found
    [here](https://github.com/input-output-hk/io-sim).


Exercises
---------

XXX: needs to be reviewed, leave debugger as exercise?

0. Add a way to record all inputs during production deployment
1. Add a way to produce a history from the recorded inputs
2. Add a debugger that works on the history, similar to the REPL from the first
   part

3. Write a checker that works on histories that ensures that the safety
   properites from section 8 on correctness from [*Viewstamped Replication
   Revisited*](https://pmg.csail.mit.edu/papers/vr-revisited.pdf) by Barbara
   Liskov and James Cowling (2012);

4. Compare and contrast with prior work:
   - https://making.pusher.com/fuzz-testing-distributed-systems-with-quickcheck/
   - https://fractalscapeblog.wordpress.com/2017/05/05/quickcheck-for-paxos/

Problems
--------

- Can we make a better DSL for expressing state machines that feels less clunky
  and is more expressive? How can we add asynchronous filesystem I/O, together
  with the appropriate fault injection, for example?

- How can we effectively explore the state space? Can we avoid exploring
  previously explored paths on subsequent test invocations? Can we exploit
  symmetries in the state space? E.g. if `set x 1 || set x 1` happen in parallel
  we don't need to try both interleavings;

- Related to the above, and already touched upon in part 4: how can we
  effectively inject faults? Is random good enough or can we be more smart about
  it? C.f. [lineage-driven fault
  injection](https://dl.acm.org/doi/10.1145/2723372.2723711) by Alvaro et al (2015);

- Can we package up this type of testing in a library suitable for a big class
  of (distributed) systems? Perhaps in a language agnostic way? So far it seems
  that all simulation testing practitioners are implementing their own custom
  solutions;

- Can we make the event loop performant while keeping the test- and
  debuggability that we get from determinism and command sourcing? Perhaps
  borrowing ideas form LMAX's
  [disruptor](https://github.com/symbiont-io/hs-disruptor/),
  [io_uring](https://lwn.net/Articles/776703/), and
  [zero-copy](https://en.wikipedia.org/wiki/Zero-copy) techniques? See the
  [TigerBeetle](https://github.com/tigerbeetledb/tigerbeetle) database for a lot
  of inspiration in this general
  [direction](https://tigerbeetle.com/blog/a-friendly-abstraction-over-iouring-and-kqueue/).

See also
--------

- ["Jepsen-proof engineering"](https://sled.rs/simulation.html) by Tyler Neely;
- The [P](https://github.com/p-org/P) programming language;
- [Maelstrom](https://github.com/jepsen-io/maelstrom);
- [stateright](https://github.com/stateright/stateright).

Summary
-------

By moving all our non-determinism behind interfaces and providing a
deterministic fake for them (in addition to the real implementation that is
non-deterministic) we can achieve fast and deterministic "end-to-end"/system
tests for distributed systems.
