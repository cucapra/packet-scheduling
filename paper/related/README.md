# Related Work

## PIFO substrate lineage

These works define and refine the hardware/algorithmic substrate that we (and vPIFO) sit on top of. They belong in §2.1.

- PIFO-Sivaraman-SIGCOMM16.pdf by Sivaraman et al., _Programmable Packet Scheduling at Line Rate_. The original PIFO paper.
- PIEO-Shrivastav-SIGCOMM19.pdf by Shrivastav, _Push-In Extract-Out_. A PIFO generalization allowing extraction from arbitrary positions; relevant if we want to discuss substrate flexibility.
- SP-PIFO-Alcoz-NSDI20.pdf by Alcoz, Dietmüller, Vanbever, _Approximating PIFO with Strict-Priority Queues_. Practical PIFO approximation on commodity switches.
- BMW-Yao-SIGCOMM23.pdf by Yao et al., _BMW Tree_. A high-throughput modular PIFO implementation; the substrate vPIFO compares against.
- Gearbox-Gao-NSDI22.pdf by Gao et al., _Gearbox: A Hierarchical Packet Scheduler for Approximate Weighted Fair Queuing_. Hardware hierarchical scheduling, a substrate alternative.
- vPIFO-Zhang-SIGCOMM24.pdf by Zhang et al., _Virtualized Packet Scheduler for Programmable Hierarchical Scheduling_. The closest related work; subject of §2.2. Their §8 explicitly leaves our transitional-phase problem open.

## Formal/PL grounding

- FormalAbstractions-Mohan-OOPSLA23.pdf by Mohan, Liu, Foster, Kappé, Kozen. Our prior work formalizing PIFO-tree semantics. The semantics of `link` (§3-onwards) builds on this.

## Scheduling-systems context

Software/NIC scheduling work that motivates the kinds of policies operators actually want to reconfigure. Useful for §1 motivation and §2.1 background.

- Eiffel-Saeed-NSDI19.pdf by Saeed et al., _Efficient and Flexible Software Packet Scheduling_.
- Loom-Stephens-NSDI19.pdf by Stephens, Akella, Swift, _Flexible and Efficient NIC Packet Scheduling_. Hierarchical scheduling at the NIC; introduces DAG-shaped policies.
- Carousel-Saeed-SIGCOMM17.pdf by Saeed et al., _Scalable Traffic Shaping at End Hosts_. Time-based shaping.
- PicNIC-Kumar-SIGCOMM19.pdf by Kumar et al., _Predictable Virtualized NIC_. Multi-tenant isolation; the kind of policy that gets reconfigured in practice.

## Programmable data planes (substrate language)

- P4-Bosshart-CCR14.pdf by Bosshart et al., _P4: Programming Protocol-Independent Packet Processors_. Cited indirectly: vPIFO targets P4, and any deployment story we tell flows through P4-runtime-style update mechanisms.

## Newer work, added 2026-09-02

Found by search rather than by hand; `paper/comments-newer-work.md` says what each is for and which are still missing.

- QVISOR-Alcoz-HotNets23.pdf by Gran Alcoz, Vanbever, _QVISOR: Virtualizing Packet Scheduling Policies_. Asks how to move seamlessly from one scheduling policy to another, and names emptying the buffers and resetting scheduling-function state as the obstacles. A second group, independent of vPIFO, calling our problem open.
- UIFO-Wang-arXiv26.pdf by Wang et al., _Programmable Packet Scheduling with Dynamic Reordering at Line Rate_. A class/packet abstraction that reorders already-buffered packets; the one recent paper that bears on our frozen-rank assumption.
- InPlaceUpdate-Namjoshi-SIGCOMM24.pdf by Namjoshi, Gheissi, Sabnani, _Algorithms for In-Place, Consistent Network Update_. Route consistency without versioned tables; the current state of the Reitblatt lineage.
- PACKS-Alcoz-NSDI25.pdf by Gran Alcoz et al., _Everything Matters in Programmable Packet Scheduling_. Approximates a PIFO's scheduling behavior and its admission control together.

Still to fetch: FlexCore (_Runtime Programmable Switches_, NSDI 22), BBQ (NSDI 24), Exp-PIFO (Proc. ACM Netw. 2025).

## Live / consistent reconfiguration

These are the closest works on _changing_ a deployed dataplane safely. They belong in a new §2.x (or fold into §2.2) on consistent updates. Reitblatt is the canonical reference for transition semantics, even though it targets SDN forwarding rather than scheduling.

- ConsistentUpdates-Reitblatt-SIGCOMM12.pdf by Reitblatt, Foster, Rexford, Schlesinger, Walker, _Abstractions for Network Update_. Per-packet and per-flow consistency for SDN updates. Strong analogue to our `link`-period semantics: they too identify that an update is not atomic and define what invariants must hold during the transition.
- IncrementalConsistentUpdates-Mahajan-HotNets13.pdf by Mahajan, Wattenhofer follow-on. Incremental variant; useful if we want to argue minimality of diffs.
- FlowBlaze-Pontarelli-NSDI19.pdf by Pontarelli et al., _Stateful Packet Processing in Hardware_. Not about reconfiguration per se, but the EFSM abstraction is relevant to how we describe `link` as a stateful intermediate policy.
