> `Quiesce (path)` stops routing any new traffic to `prev@path`. It is a transaction-only edit...

The existence of `Quiesce` suggests that the "domain" of edits is not only a tree but also the associated control tuple. So I think that means that diffs are not tree-to-tree functions but X-to-X functions for some larger value of X---probably the control tuple, `(s, q, z)`. If so, let's try to make that clear: e.g., at the beginning of Section 3.2, when we refer to `prev` and `next`, those things have to be control tuples, not trees. (The text there currently refers to trees.)

There may also be an alternative where we say that we only want to apply diffs to trees (not control tuples)? Which would mean that `Quiesce` exist, and we'd have to handle that at a different level of abstraction.

> Denotation
> The diff

Also related to my confusion around domains: I am not sure what the difference is between "denotation" and "the diff" is. Maybe the former is just about the tree, and the latter is about the whole control tuple?

If so, I get why the latter exists (we need to describe how to change the state, for example). I admit I am a little wary of the subtle-but-informal descriptions of these manipulations, such as "`s'` agrees with `s` everywhere, except that it records the initial local state for the new slot." This seems like something that we probably want to describe formally, in math? Is that too much to shoot for?
