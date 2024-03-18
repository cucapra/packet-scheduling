
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
