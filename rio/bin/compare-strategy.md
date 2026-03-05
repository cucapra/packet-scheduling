This document seeks to write out the strategy for comparing two policies, as implemented in `compare.ml`. So far we have been pretty example-driven, but now we've warmed up enough that we can write out the strategy in general. We're clearly not just an AST v/s AST comparison of the kind that `yojson` can do. Rather, our comparison needs to understand a little about the semantics of certain policies. Our challenge is to articulate the strategy as generically as possible, while nailing down precisely what is required of the policies of interest.

I will denote policies with `Q`, `R`, `S`, programs with `p1` and `p2`, and classes of traffic with `A`, `B`, `C`.

We start with programs `prog1` and `prog2`.

### Parse them into AST policies `p1` and `p2`, desugaring as we go:

- We desugar any instance `Q[..., A, ...]` into `Q[..., fifo[A],...]`. This means that simply `A` is a valid policy because `A = fifo[A]` and simply `fifo[A]` is a policy.
- We desugar any instance `union[Q,R,...]` into `fifo[Q,R,...]`. This is perhaps a little silly and we can get rid of it eventually.

We have some basic error-checking in place, e.g., using a class that was not declared at the top of the program, using a class twice, etc. It may not be the most rigourous, but anyway I won't get distracted by that in this document.

### Normalize the ASTs

We cannot just normalize the ASTs naively, since some policies bake semantic meaning into the _order_ of their children. E.g. `SP[A,B] != SP[B,A]`, so it is a mistake to "sort" the latter's children lexicographically and turn it into the former.

- For those policies that care about the order of their children (presently SP only), we recursively normalize each _child_ of that node but _do not_ sort the normalized children.
- For all other cases, we recurively normalize each child _and_ sort the normalized children. Presently this sorting is just done using OCaml's `compare`, which is relatively opaque. This can easily be changed to something we understand better, if we can motivate a choice. Some other notes:
  - For FIFO and EDF, which are set-to-stream policies having _classes_ as children, we do a lexicographical sort of the string names of the classes. So `fifo[C,A,V]` is always normalized to `fifo[A,C,V]`. Semantically this is fine, as the children of a FIFO or EDF node are union-ed together anyway.
  - For WFQ, which has _two_ lists---a list of classes and a corresponding list of weights---we sort the lists in lockstep, guided by how `compare` compares the _classes_, not the weights. So `wfq[(B,A),(30,70)]` is normalized into `wfq[(A,B),(70,30)]` because `compare` would sort `B,A` into `A,B`.

### Check if the two policies are the same

Examples include programs that were literally syntactically same, and also programs that have been releaved to be the same after parsing and normalization.

### Search for obvious sub-policies

We believe it will be easy to go from `p1` to `p2` where `p2` is just `Q[...,R[..., p1,...],...]` or somesuch. That is, `p1` occurs _exactly_ within `p2`. So we first go off and search `p2` to see if `p1` occurs exactly. Thanks to our previous normalization, this step can actually be a totally generic search for a tree within another tree.

For now we do something slightly more sophisticated: we find _where_ in `p2` we see `p1`. We return a `path : int list`, which is the series of indices one would follow, starting from `p2`'s root, to get to `p1` within `p2`.

### TK: Search for non-obvious sub-policies

### Recurse when the root policy is the same

If `p1` and `p2` have the same root node policy `Q`, we recursively check their children to find exactly where the policies diverge. For now our main change of interest is _arms being added_ whilst leaving the earlier arms as they were. If arms are removed, we give up. If