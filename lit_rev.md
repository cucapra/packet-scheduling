### FlowValve: Packet Scheduling Offloaded on NP-based SmartNICs
Xi et al at DCS '22 | [Paper](https://ieeexplore.ieee.org/document/9912227)

Major goals:
- Parallel, flexible packet scheduling on SmartNICs. Restricted to Network Processor (NP)-based SmartNICs. 
- They wish offload Linux features onto the SmartNIC. This is hard because:
	- The naive offload gives low throughput. They develop parallel scheduling algorithms and new data structures.
	- Naively offloading _traffic shaping_ is expensive, since that requires buffering and resending packets. They simulate shaping via dropping.
	
Citeable facts:
- Not citeable, but just a good sign: they have an example of a hierarchical scheduling policy in their motivation section! It mixes work conserving and non work conserving policies.

Further reading: 
- Network Processor (NP)-based SmartNICs. §III-B has some info though.
- They say that PIEO-based schedulers overcome limitations on Sivaraman et al's PIFO trees at dequeueing by supplementing filters to express a wider range of algorithms. https://web.ics.purdue.edu/~vshriva/papers/pieo_2019.pdf

Questions:
- Don't totally get their specialized tail drop. 

They abstract over the existing queues on a NIC to create a logical FIFO. They "perform specialized tail drops to mix the FIFO queue with expected flow proportions". I'm not totally sure what they mean, but the line is: "Unlike common tail drop, FlowValve prejudges which packet would cause buffer overflow to its belonged traffic class. Then it explicitly drops this packet in advance. In this way, FlowValve assigns buffers conceptually."


### Shinjuku: Preemptive Scheduling for μsecond-scale Tail Latency 
Kaffes et al at NSDI '19 | [Paper](https://www.usenix.org/system/files/nsdi19-kaffes.pdf)

Major goals:
- The issue is that workloads with high dispersion (packets get spread out to many workers, essentially the opposite of affinity to one core) and workloads with heavy tails (many small packets after a few big packets) get very poor service. This paper proposes a centralized scheduling mechanism featuring Linux-style _preemption_ but at _very fine granularity_. This gives higher throughput and lower latency, thereby preventing the kinds of blocks we were seeing.
- The fine granularity of preemption is achieved by lowering the preemptions to the hardware level. Moving to hardware is not a cure-all, and incurs its own slowdown. Some further jankiness, e.g. sharing of address spaces and "posted interrupts", are needed in order to avoid the slowdown that hardware would itself incur.
- In reality they maintain a small number of FIFOs, one per "class" of packets. They have come up with a lightweight policy that determines which of those FIFOs should be popped next. The policy takes into account how long ago each FIFO was popped, and what service-level objectives the user stated for the class of packets in each FIFO. A preempted packet is reinserted either at the head or the tail of its queue, and this has varying effects. This can be modified based on the desired scheduling regime.
- The end result is a scheduler that simulates cFCFS when the workload is easy, and PS when the workload is challenging. See below for what those policies are.
- Note that this work is an opinionated departure away from the RSS-style work we've seen above. In their words: "If service times exhibit low dispersion and there are enough client connections for RSS to spread requests evenly across queues, stealing happens infrequently."
- Overall I'm a fan. This is a top-notch paper that does some very helpful surveying of the field in addition to its own strong contribution. It has also been used extensively by RackSched (see below).
- The [video](https://www.usenix.org/conference/nsdi19/presentation/kaffes) is a worth a watch too.

Citeable facts:
- Centralized first-come-first-serve (cFCFS), where you maintain one central FIFO that any worker can pop to grab its next piece of work, is near-optimal for low-dispersion workloads.
- Processor sharing (PS), where all requests receive a fine-grained and fair fraction of the available processing capacity, is near-optimal for heavy-tailed workloads or light-tailed workloads with high dispersion.

Further reading: 
- They say that they want to integrate with Shenago. Has something along those lines happened since 2019?

Questions:
- Can we really presume that all this work is preemptible? 
- Even if so, can a task get stuck forever, with barely any work getting done on it? They mention that this hurts them in one experiment. Maybe an interesting contribution could be that a task gets additional "karma points" every time it is preempted and sent back. Concretely, this could mean (a combination of):
	- Putting it at the head of its FIFO, not at the tail.
	- Making the preemption logic aware of the packet's many prior trips, so this packet in particular is preempted less eagerly.
	- Maintaining a dedicated core that is preempted less eagerly, and passing oft-preempted packets to that core. This idea almost reminds me of hierarchical garbage collection: young blocks die quickly, old blocks stick around. We could even have a whole spectrum of cores with different preemption eagerness settings, along with some notion of promotion instead of just re-sending to the same core.


### RackSched: A Microsecond-Scale Scheduler for Rack-Scale Computers
Zhu et al at OSDI '20 | [Paper](https://www.usenix.org/system/files/osdi20-zhu.pdf)

Major goals:
- They just want to build a huge machine: the entire rack operating essentially as one. 
- The individual servers themselves run Shinjuku for intra-server scheduling. This paper's contributions are in the higher-level inter-server scheduling space. 
- This inter-server scheduler does load balancing and honors request affinity. It also tracks server loads. 
	
Citeable facts:
- Mostly just a nice example of a paper that is trying to schedule servers with a ToR switch, analogously to how we are trying to schedule cores with a SmartNIC.
- This is the first of many time we'll see RocksDB used in evaluation. It's a KV service with short GETs and long SCANs. You can craft experimental workloads by modulating how many GETs and SCANs a flow of requests has

The Shinjuku paper has shown that cFCFS and PS are ideal policies in many circumstances. The Shinjuku system also approximates these policies at the intra-server level. This paper finds that running JSQ (join the shortest queue) at the _inter_-server level actually approximates cFCFS/PS at the inter-server level. That is, the entire rack appears to run Shinjuku's cFCFS/PS.

Pretty cool! But one wonders what the limitations of this observation are... we first commit to a server and then let the server run its buffering and scheduling routines using Shinjuku. Surely there is a flexibility cost to this? Shinjuku's secret sauce was preemption, and that is not possible at the inter-server level?

## TODO

### Shenango: Achieving High CPU Efficiency for Latency-sensitive Datacenter Workloads
Ousterhout et al at NSDI '19 | [Paper](https://www.usenix.org/system/files/nsdi19-ousterhout.pdf)
 
### Loom: Flexible and Efficient NIC Packet Scheduling
Stephens et al at NSDI '19 | [Paper](https://www.usenix.org/conference/nsdi19/presentation/stephens)

Others cite this paper as the SOTA on programmable packet scheduling on NICs. Note, un-smart, so it would take years to tape out onto ASICs.

### SENIC: Scalable NIC for End-Host Rate Limiting
Radhakrishnan et al at NSDI '14 | [Paper](https://www.usenix.org/system/files/conference/nsdi14/nsdi14-paper-radhakrishnan.pdf)

### Eiffel: Efficient and Flexible Software Packet Scheduling
Saeed et al at NSDI '19 | [Paper](https://www.usenix.org/conference/nsdi19/presentation/saeed)

### A large-scale deployment of DCTCP
Dhamija et al at NSDI '24 | [Paper](https://www.usenix.org/system/files/nsdi24-dhamija.pdf)

See §4.4

#### OS Scheduling: Better scheduling policies for modern computing systems
Kaffes in CACM | [Paper](https://dl.acm.org/doi/pdf/10.1145/3595837)

A review of the SOTA in OS scheduling by the Shinjuku lead author!
