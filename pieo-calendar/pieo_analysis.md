# PIEO Queues – Overview & Analysis

This serves to discuss the implementation of the PIEO (Push-In-Extract-Out) Queue, its relevance against PIFOs and potential drawbacks.

### Papers

1) [DR-PIFO: A Dynamic Ranking Packet Scheduler Using a Push-In-First-Out Queue](https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=10215378&tag=1) – Elbediwy et al 2024, IEEE
2) [Discussion on Push-In-Extract-Out Scheduler in Hardware](https://cemclaughlin.github.io/img/portfolio/PIEO%20Presentation/Final%20Project%20Report.pdf) – McLaughlin, University of Hawaii

### Summary

Fundamentally, a PIEO (Push-In Extract Out) Queue is a generalization of the PIFO (Push-In First Out) Queue which we have already studied. The main difference lies in how a queue is queried.
### Functionality and Relevant Operations

PIFOs contain two key functions – `enqueue(f)` and `dequeue(f)` (or, `push` and `pop`), alongside the query method `peek`. These require `f` to be some element containing a value and a rank.

With PIFOs, enqueueing pushes a new packet `p` into a queue such that the rank ordering is preserved. Dequeueing pops the head of the queue.

PIEOs embody all of this functionality, but with an added **eligibility predicate**. This takes on the form of a partial function mapping PIEO nodes to **true** or **false**. (See [McLaughlin](https://cemclaughlin.github.io/img/portfolio/PIEO%20Presentation/Final%20Project%20Report.pdf)).

`enqueue` functions the same way. The `dequeue` function can take in an optional parameter `p` representing a specific packet to be dequeued.

If such a parameter is provided and and `p` occurs within PIEO, then the packet `p` is dequeued and returned, regardless of rank and eligibility. Otherwise, `None` is returned.

Notably, in the case that one wishes to update the value or rank of a packet `p`, this can be performed by dequeueing `p`, updating it, and then enqueueing it again.

If no parameter is provided, then the PIEO filters all packets such that the eligibility predicate returns **true**. It the returns the element with the lowest rank. In the case of a tie, the earliest element to be enqueued is returned.

**A Side Note/Question On Peeking**

While both papers have only explicitly discussed the `enqueue` and `dequeue` functions (with no mention of peeking), our PIFOs account for **peeking** as well.

I propose that for PIEOs, we generalize the function `peek` in the same way we will `pop` – that is to say, we run `dequeue` with optional parameter `p`, but do not modify the PIEO in question.

### Pros and Cons of PIEOs (Against PIFOs)

There are a few drawbacks of using a PIFO for packets, which PIEO queues can address (See in particular Section 2E of [Elbediwy et al](https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=10215378).)

**Inability to Re-Rank Enqueued Packets**
- Once a packet has been enqueued in a PIFO, it cannot have its rank modified, as there is no way to re-access it through the PIFO's interface.
- This is easily achievable with a PIEO, as described above. By `dequeueing` a specific packet `p`, modifying it as desired and then `enqueueing` it again, we're able to bypass this issue.

**Scalability**
- A major issue with PIFOs is that it is unable to handle high numbers of flows in a reasonable timeframe (Elbediwy et al upper bounds this at around 1000). 
- PIEOs can instead handle up to 30,000 simultaneous flows by optimizing its register usage.

**A Potential Downside – Speed**
- PIEOs require a four-step non-pipelined system, which is slower than the standard PIFO architecture.
- Enqueueing and dequeueing achieve a throughput of 1 packet per 4 cycles, while updating a rank can lead to an 8-cycle break from the norm.

### A Visual OCaml Representation

For the sake of argument and a visual aid, I've explored a functional PIEO module type implementation in OCaml:

```ocaml
module type PIEO = sig
	type rank
	type 'a t
	val eligibile: 'a -> bool
	val enqueue: 'a t -> 'a -> rank -> 'a t
	val dequeue: 'a t -> 'a option -> ('a option * 'a t)
	val peek: 'a t -> 'a option -> ('a option * 'a t)
end
```

In which `eligible` maps any element to **true** or **false**, `enqueue` takes in a PIEO, element and rank and returns a PIEO, and both `dequeue` and `peek` take in a PIEO and an optional element, and either return the element (if found) and a PIEO queue.