Simulation testing
==================

![](../images/under_construction.gif)

*While the overall structure and code will likely stay, there’s still more work
 needed to turn this part from a bullet point presentation into a readable
 text.*

Motivation
----------

For many systems the combination of integration tests and (consumer-driven)
contract tests together with smoke tests should yield a test-suite with "good
enough" coverage.

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

So in this part we'll have a look at how FoundationDB is tested.

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

This would allow us to simulate a network of components by connecting the fakes
and let them "communicate" with each other. The fake time could be advanced
discretely based on when messages arrive rather with a real clock, that way we
don't have to wait for timeouts to happen.

While the software/system under test (SUT) is running in our simulator we hit it
with client requests and collect a concurrent history, after the simulation is
done we check if the history linearises (like in [part
2](./Part02ConcurrentSMTesting.md#readme)). Other global assertions on the state
of the whole system are also possible.

To make debugging easier we'll also write a time-travelling debugger that lets
us step through the history of messages and view how the state of each state
machine evolves over time.

How it works
------------

- Given that all the fakes our components are state machines of type `Input ->
  State -> (State, Output)`, we can "glue" them together to form a network of
  components where the outputs of one component gets fed into the input of
  another and so on. This is particularly effective for distributed systems
  where we have N instances of the same component and they typically all talk to
  each other;

- Further note that since the networking part is already factored out of our
  state machines, all we need to do is to write an event loop which does the
  actual receiving and responding of messages and feeds it to the state
  machines. The picture looks something like this:

  ![simulation event loop](../images/simulation-eventloop.svg)

  where client requests (synchronously) and internal messages from other nodes
  in the network (potentially asynchronously) arrive at the event loop, get
  queued up and then dispatched to the state machine running on the event loop.
  After processing a message (the green arrow) the event loop updates the state
  of the state machine and sends out any replies.

- In order to reuse as much code as possible between real production deployment
  and simulation testing we can parametrise the event loop by an interface that
  does the delivering and sending of the network messages (the blue boxes in the
  diagram), and merely swap those out depending on which mode of deployment we
  want.

  For the real deployment send and deliver actually use the network to send and
  deliver messages to clients or other nodes in the network, while in the
  simulation testing deployment send generates a random (but deterministic via a
  seed) arrival time and inserts the message in a priority queue sorted by time,
  deliver pops the priority queue and steps the appropriate state machine with
  the next message and inserts all the replies back to the queue, rinse and
  repeat. Another difference between the real and simulation testing deployment
  is that in the real case we want to run one node per event loop, while in
  simulation we can run a whole network of nodes on one event loop.

- Injecting faults can be done on the event loop level while simulation testing,
  e.g. dropping random messages from the priority queue or introducing latency
  by generating arrival times with a greater variance.

- During simulation testing the time is advanced upon delivering a message, e.g.
  if an incoming message has arrival time T then we first advance the time of
  the state machine to T and then feed it the message, by advancing the time we
  might trigger various timeout and retries without actually having to wait 30s
  (or whenever).

Code
----

> module Part05SimulationTesting () where
> import Part05.EventLoop ()

Discussion
----------

- What are the risks of simulation testing being wrong somehow?

  + We've made an assumption: each message is processed atomically, a more
    fine-grained approach where each state machine has a "program counter" that
    gets incremented is imaginable, but will introduce a lot more complexity and
    many further states;

  + Even though we tried to minimise difference between "production" and
    simulation testing deployment there's always going to be a gap between the
    two where bugs might sneak in;

  + Are our faults realistic and complete? A good source for inspiration for
    faults is Deutsch's [fallacies of distributed
    computing](https://en.wikipedia.org/wiki/Fallacies_of_distributed_computing).
    Jepsen's list of nemesis, the Chaos engineering communties faults and
    FoundationDB's simulator's faults are other good sources;

  + Are we subconsciously training users of the simulation tests to beat it? See
    relevant part of Will Wilson's [talk](https://youtu.be/4fFDFbi3toc?t=2164).

- Even if we are reasonably sure that we've mitigated all the above risks, we
  will be facing the state space explosion problem. There's simply too many ways
  messages can get interleaved, dropped, etc for us to be able to cover all
  during testing.

  The only way around the state explosion problem is to use proof by induction.
  A simple analogy: we cannot test that some property holds for all natural
  numbers, but we can in merely two steps prove by induction that it does. For
  more on this topic see Martin Kleppmann's
  [work](https://lawrencecpaulson.github.io/2022/10/12/verifying-distributed-systems-isabelle.html);

- Where do these ideas come from?

  + The first reference we been able to find is the following concluding remark by
    Alan Perlis (the first recipient of the Turing Award) about a discussion about
    a paper on simulation testing by Brian Randell at the first
    [conference](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF) on
    Software Engineering (this is where we got the term from) in 1968:

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

  + The idea in its current shape and as applied to distributed systems was
    introduced(?), or at the very least popularised, by Will Wilson's talk
    [*Testing Distributed Systems w/ Deterministic
    Simulation*](https://www.youtube.com/watch?v=4fFDFbi3toc) at Strange Loop (2014)
    about how they used [simulation
    testing](https://apple.github.io/foundationdb/testing.html) to test
    [FoundationDB](https://www.foundationdb.org/) (so well that Kyle "aphyr"
    Kingsbury didn't feel it was
    [worth](https://twitter.com/aphyr/status/405017101804396546) Jepsen testing
    it).

    Watching the talk and rereading the Perlis quote makes one wonder: was the
    technique independently rediscovered, or had they in fact read the (in)famous
    1968 NATO software engineering report?

  + There's also the more established practice of [discrete-event
    simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation) which is
    usually used in different contexts than software testing, but nevertheless is
    close enough in principle that it's worth taking inspiration from (and indeed
    the simulation testing people often refer to it).

  + John Carmack wrote an interesting
    [.plan](https://raw.githubusercontent.com/ESWAT/john-carmack-plan-archive/master/by_day/johnc_plan_19981014.txt)
    about recoding and replaying events in the context of testing in 1998, and
    other
    [developers](http://ithare.com/testing-my-personal-take-on-testing-including-unit-testing-and-atddbdd/)
    in the the game industry are also advocating this technique;

  + AWS: here's a quote from the recently published paper [Millions of Tiny
    Databases](https://www.usenix.org/conference/nsdi20/presentation/brooker) (2020)
    written by three AWS engineers:

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

  + Dropbox has written several blog posts related to simulation testing:
    * https://dropbox.tech/infrastructure/rewriting-the-heart-of-our-sync-engine
    * https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine
    * https://dropbox.tech/infrastructure/-testing-our-new-sync-engine

  + Basho's
    [Riak](https://speakerdeck.com/jtuple/hansei-property-based-development-of-concurrent-systems)
    (a distributed NoSQL key-value data store that offers high availability,
    fault tolerance, operational simplicity, and scalability)

  + IOHK: from their recent
    [paper](http://www.cse.chalmers.se/~rjmh/tfp/proceedings/TFP_2020_paper_11.pdf)
    "Flexibility with Formality: Practical Experience with Agile Formal
    Methods in Large-Scale Functional Programming" (2020):

   > "Both the network and consensus layers must make significant use of
   > concurrency which is notoriously hard to get right and to test. We
   > use Software Transactional Memory(STM) to manage the internal state
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

- How can we effectively explore the state space? Can we avoid exploring
  previously explored paths on subsequent test invocations? Can we exploit
  symmetries in the state space? E.g. if `set x 1 || set x 1` happen in parallel
  we don't need to try both interleavings;

- Related to the above, and already touched upon in part 4: how can we
  effectively inject faults? Is random good enough or can we be more smart about
  it? C.f. [lineage-driven fault
  injection](https://dl.acm.org/doi/10.1145/2723372.2723711) by Alvaro et al
  (2015);

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
  [tigerbeetle database](https://github.com/tigerbeetledb/tigerbeetle) for a lot
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

By moving all our non-determinism behind interfaces and providing a deterministic
fake for them (in addition to the real implementation that is non-deterministic)
we can achieve fast and deterministic end-to-end tests for distributed systems.
