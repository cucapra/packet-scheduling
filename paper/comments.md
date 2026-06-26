> A _topology_ `t` is a finite tree carrying no data

Unless I missed it, we don't use this anymore in the rest of the paper. Do we need it?

> [AM note: this is still stated in the big brain model. If we want to state it in the small-brain model, I can, but that is not exactly faithful to Mohan et al (see line just below).]

All this depends on the theorems we want to state. To the extent the theorem needs a different approach, we'll need to change tactics.

One somewhat odd thing here is that the formalism so far doesn't talk at all about state or scheduling transactions (i.e., the "control" construct). In other words, a PIFO tree as described here is just a data structure for containing packets and performing pushes/pops; deciding what to push or pop where is not its concern.

We *could* fix this with a forward reference, but I actually think we probably want a little bit of a reorg: let's move our "control" formalism upward, into sec 3.1. The current division of labor could be a little confusing because 3.2 is both about the compile-time policy language _and_ its run-time implementation in controls; sec 3.1 is about the other part of the run-time implementation. Making 3.1 about the run-time implementation and 3.2 only about the policy language for defining it might be a little cleaner? It would also make the "discipline compilation" stuff easier to read, because we will have already seen the _target_ of this compilation.

> `WFQ` takes a positive real weight per arm; `Strict` takes a priority rank per arm; `RoundRobin` takes nothing. The grammar carries no structural mark of any of this.

Maybe we want a second grammar that _does_ include this (i.e., to fully describe the "surface syntax")? Just to make it as clear as possible what the surface syntax looks like. It just seems a little weird to have aspects of the source code that _is relevant to compiling down to a control_ not actually appear in the source programs.

> For instance, a compiler might run a deterministic `normalize` operation, sorting `Strict` arms by rank, `RR` arms by content, and so on, then commit to that one representative throughout.

FWIW, I don't think we need much discussion here about the order-insensitivity; it's pretty intuitive why we want it.

> A `pol` is written against a fixed _flow universe_ `F`

In that case, please add the set `F` to the grammar: as in, write `flow \in F` if we indeed want `flow` to be the metavariable for a flow.

> In §3.1 we defined well-formedness on a PIFO tree, written `|- q`. Now we redefine it, lifting it to act on controls.

Let's not say we're "redefining" it; we are just defining wellformedness for controls (where we previously gave a separate well-formedness condition for PIFO trees). It is fine to use the same symbol for two different definitions if it's clear from context which we're using.

> A control of the kind we have just defined is sometimes packaged instead as a single _monolithic triple_ `(s, q, z)`:

I think what this sections wants to say is: "Notice that our setup is slightly different from Mohan et al. in the following way. We think this is no big deal because you can convert ours to theirs in a straightforward way."

I think this can probably be conveyed in 1–2 sentences within the section that introduces control?

> We write `C ~ C'` for the equivalence relation on well-formed controls that identifies any two controls related by a finite sequence of `push` / `pop` operations.

Would it work (i.e., would our theorems still be true) to just say that `C` and `C'` are equal except for the PIFO contents and states? This is a weaker statement, because it would allow controls to be related even when their state/contents are "infeasible." But I am sorta guessing that this does not matter for our theorems?

> The bridge: `⌊·⌋`

IIUC, we do not want condition 3 here. (To recap: the other conditions define `⌊·⌋` to be the inverse of `⌈·⌉` modulo `~`. Condition 3 says something about `δ`.) It seems to me that this is a consequence of the definition and should not be part of the definition. As in, everything involving `δ` is a theorem about how policy-differences relate to control-differences. (Unless I'm missing something.)

Separately: I think we should try to be clear about whether `⌊·⌋` is a function. That is, for every `C`, is there a single _unique_ policy `pol` such that `⌊C⌋ = pol`? I am guessing that this is true up to reordering, but let's try to be certain. (Because, otherwise, we can't use it like a function.)

> The interplay of the three rules is captured by the following diagram.

I think this stuff needs to move after the introduction of `δ`, `den(δ)`, and `[[δ]]` in order to be fully intelligible to the reader. As in, perhaps we want a new Section 4 that comes after Section 3 (grammar of diffs) that exists solely to state this theorem.

I also think we should state this as a theorem, i.e., in math text. The figure can help build intuition about what the theorem means, but the real "source of truth" will be the textual theorem statement.

> Preservation of observation.

I'm a little confused about this goal. It says that applying a diff to a control doesn't change what you pop from it. ("a `pop` immediately after `δ` fires returns exactly what a `pop` immediately before would have returned") But don't we want diffs to affect the "pop stream"? Like, if you invert the weights in an `SP`, surely that means that you are changing the next packet to be popped.

I guess I'm not sure how this is related to atomicity. At this level of abstraction, all "atomicity" means is that we can model the semantics as operations on `C`s, i.e., execution consists of a stream of `push`, `pop`, and `δ` operations, each of which transform a `C` into another `C`.

> 4.1 Guarded Sequences

I kinda think we want to choose whether to make this section more formal or less formal. It's currently somewhere in between; we're giving a formal syntax for guarded sequences and explaining in some detail how to "execute" them on a given control (and even writing down some theorem-like statements), but never actually formalizing an operational semantics for step-by-step application. (For example, conditions φ have no defined semantics (doing so would require developing quite a bit more machinery); we just say informally what they do.)

My suggestion is that we choose to make this less formal. The point of this section is the "idioms," i.e., to show _by example_ that it's possible to elicit some "interesting" high-level tree changes by composing our low-level diffs. Let's embrace the fact that this is just a vehicle for presenting those examples and tone down the ceremony. This might end up making things clearer, and there's a strategic advantage too: it will avoid getting people mad if they expect this to be more fully formalized than it is.

IMO what this means is:

* keep the grammar for φ and gseq but delete the grammar for I
* compress the discussion of what these "mean" to just a few sentences getting at the intuitive idea, i.e., we have made hardware that can wait for φ and then run δ, and we just do that several times
* delete "Sequence semantics", "The iterated picture", "Safety", "Liveness" (perhaps the latter deserves a footnote-level mention, but not a section)
* dedicate most of the text to giving the "code listings" for each of the idioms, and probably illustrating each with a picture

> 4.3 Authoring modes
> 4.4 Handling follow-up requests

I'm not entirely sure where to put this discussion, but it seems a little out of place here. At a high level, this is the "UX" stuff, i.e., trying to explain how an operator might use the language+hardware we are selling in this paper. There is not much technical to say about this; we're just explaining how to think about the technical contributions. My best guess is that the right organization doesn't have sections about this—we just make it clear how this works in the intro or some kind of overview. Is that too brash of a suggestion?

> 5. The Transition Planner

Skipped, per your suggestion.

> 6.1 The commit model and the ISA

Why do all the opcodes start with `ISA_`? If the point is to distinguish between similarly-named δ cases and opcodes, I suggest we can do that with typography instead. (For example, maybe we typeset the ISA instructions as code, whereas δ and stuff is math.)

> 6.2 Lowering Productions of δ

Let's retitle this to "Compiling Policy Diffs."

Overall, I think this section will be more readable if we—as much as possible—make the compiled output for each δ look like a code listing, with as little English as possible. Currently, it can be a little hard to extract from the bulleted list what the ISA "program" must look like. Can we format the result as a block of code (with comments, if necessary) in most cases? (And not as a bulleted list of English text surrounding some code?)

Of course, the problem with that suggestion is in the cases where we need some "meta-level" logic for generating code. The simplest such situation is when you need to emit N copies of the same chunk of code for all of the children of a given node or whatever. (Such as the last bullet point in the `Add` case.) For this, I think we should invent some more "code generation notation," of the same flavor as `walk`. So our "code listings" will actually look like sequences of ISA instructions, sprinkled in with these meta-level directives for making copies and stuff. Even so, I think making these _look like code listings_ as much as possible will make it much easier to understand the shape of the code we're generating in each case.