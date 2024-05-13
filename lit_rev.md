
#### Background 

A data center acts like a multiplexer, matching tasks ("packets") with CPUs that can accomplish those tasks ("cores"). The tasks may be time sensitive or may be "batch mode" style work that is not as time-sensitive. Doing this multiplexing at the NIC makes sense, since every packet must go through the NIC anyway. SmartNICs to the rescue! That said, a smartNIC cannot do this scheduling in a vacuum; it needs to coordinate with the CPU cores, which have additional info. 

#### Elastic RSS
Rucker et al at APNet '19 | [Paper](https://ppl.stanford.edu/papers/apnet19_erss.pdf)

Major goals:
- Don't dedicate a CPU core to TM, as this will bottleneck your throughput. 
- Schedule the traffic intelligently, with the ability to respond to upstream changes. 

Minor goals:
- Line rate operation
- Work conservation
- CPU utilization, e.g. don't dedicate a core to TM, handle bursts without leaving cores underutilized, etc.
- Dispersion tolerance
- Packet stealing

Citeable facts:
- Real-world tail latency is 100 microseconds.
- Packet stealing improves tail latency in case of head-of-line blocking at a core.

Further reading: 
- Taurus [19].
- Possible to make updates to flow tables over the PCIe bus using Programmed IO [15].

Questions:
- How dated is this? 
- Are we interested in Taurus?

This paper argues that packets and cores should be co-scheduled at the NIC. They want to use Taurus, a programmable NIC, which has a map-reduce abstraction. Map: for each packet, find the weighted consistent hashing distance to each core. Reduce: for each packet, find the closest allocated core. 

Scheduling is at two timescales: fine-grained, per-packet processing at NIC, coarse-grained state management at CPU. 


#### PANIC
Lin et. al. at OSDI '20 | [Paper](https://www.usenix.org/system/files/osdi20-lin.pdf)

Major goals:
- Offload variety: hardware and software offloads
- Offload chaining: avoid wasting area on redundant functionality. Instead, provision the functionality in one place, and cleverly chain tasks to get them consumed by the right core. 
- Multi-tenant isolation: tenants should not be able to consume more than their allocation of a shared offload. Required on-the-fly reprogramming of the TM. 
- Variable-perfomance offloads: latency sensitive, or not. 
- Line rate

Citeable facts:
- Categorization of existing NIC designs into three kinds, along with their limitations when it comes to the five major goals above. 

This paper contributes a new NIC design that combines a variety of offloads into chains that can then be consumed efficiently by a pipelined core. 

They want the design to be in 4 parts: 
- RMT pipeline that makes packets into chains
- Fast switching fabric that connects everything
- Scheduler
- Cores, each running one offload

Packets are given a "PANIC descriptor", then pushed to the scheduler, which then buffers the packet until the first destination core is idle. It then pushes the packet to that idle core. A core may further push the packet directly to another core, without going back to the scheduler. If a core receives a packet while not idle, it may push the packet to the scheduler's buffer and then pull it later on. You more or less reverse this for transmission. 



#### AlNiCo
Li et. al. at USENIX ATC '22 | [Paper](https://www.usenix.org/system/files/atc22-li-junru.pdf)

Major goals:
- Contention awareness: minimize inter-transaction contention over cores. 

Citeable facts:
- Two existing methods of scheduling, broadly. (Section 2.2). These are:
    - Static data partitioning, where the client knows a partition scheme and sends its packets to some known core directly. Con: can't handle cases that are dynamic, or that don't partition nicely. 
    - Batching-based scheduling, where worker threads dynamically collect a batch of transactions, divide the batch into groups, and minimize contention between groups. Con: takes time to make the groups. 
- FPGA-armed smartNICs are either "on-path" or "off-path". Off-path is what this paper (and, indeed, we) are thinking of: traffic flows through the NIC as normal, and some of that traffic may be sent to the FPGA using a PCIe link (Section 2.3).

Questions:
- I don't fully get how their "feedback mechanism" (between the upper-level transaction software and the smartNIC) works (Section 3.3).

This paper highlights two challenges with transaction scheduling:
- The metrics by which we want to schedule a packet may be sophisticated and dynamic, not just a 5-tuple.
- Calculating which CPU core is best suited to deal with a packet, without resource contention, is hard and takes up cycles. 
They have a new way to compactly representing the state of contention (a function of the "request state" of the packet, the "worker state" of each CPU, and the "global state" of the whole server), which allows them to calculate the possibility of contention at the hardware level. 

Contention (where two transactions access the same record, and at least one of those transaction is a write) is bad because it leads to an abort. Aborts can cascade. That said, we cannot aim for perfect contention-awareness all the time, since that would slow us down too much. This paper seeks to minimize this contention without slowing things down.

Clients of AlNiCo must tag packets with a fixed-form header called the "request feature vector". The data plus the header is sent to the scheduler. The scheduler looks at these, plus the state of the worker threads, and notifies the worker threads of their next tasks. This notification is just an address for the data, not the data itself. When the thread is ready, it pulls the data from a buffer, does its work, and transmits the answer to its client.

#### FlowValve: Packet Scheduling Offloaded on NP-based SmartNICs
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


#### Shinjuku: Preemptive Scheduling for μsecond-scale Tail Latency 
Kaffes et al at NSDI '19 | [Paper](https://www.usenix.org/system/files/nsdi19-kaffes.pdf)

TK: will transfer paper notes.

Citeable facts:
- Centralized first-come-first-serve (cFCFS) is near-optimal for low-dispersion workloads, and processor sharing (PS) is near-optimal for heavy-tailed workloads or light-tailed workloads with high dispersion.


#### RackSched: A Microsecond-Scale Scheduler for Rack-Scale Computers
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


## TODO: 

#### Shenango: Achieving High CPU Efficiency for Latency-sensitive Datacenter Workloads
Ousterhout et al at NSDI '19 | [Paper](https://www.usenix.org/system/files/nsdi19-ousterhout.pdf)
 
#### Loom: Flexible and Efficient NIC Packet Scheduling
Stephens et al at NSDI '19 | [Paper](https://www.usenix.org/conference/nsdi19/presentation/stephens)
Others cite this paper as the SOTA on programmable packet scheduling on NICs. Note, un-smart, so it would take years to tape out onto ASICs.

#### SENIC: Scalable NIC for End-Host Rate Limiting
Radhakrishnan et al at NSDI '14 | [Paper](https://www.usenix.org/system/files/conference/nsdi14/nsdi14-paper-radhakrishnan.pdf)

#### Eiffel: Efficient and Flexible Software Packet Scheduling
Saeed et al at NSDI '19 | [Paper](https://www.usenix.org/conference/nsdi19/presentation/saeed)

#### A large-scale deployment of DCTCP
Dhamija et al at NSDI '24 | [Paper](https://www.usenix.org/system/files/nsdi24-dhamija.pdf)
See §4.4

#### OS Scheduling: Better scheduling policies for modern computing systems
Kaffes in CACM | [Paper](https://dl.acm.org/doi/pdf/10.1145/3595837)
A review of the SOTA in OS scheduling by the Shinjuku lead author!
