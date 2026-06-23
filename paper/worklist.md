# Worklist: open items for sketch.md and rio/

Consolidates the former `review.md` (sketch.md prose review), `divergences.md` (paper/code worklist), and `planner-gaps.md` (planner coverage gaps).
Closed and deliberately-left items from those files are dropped; only what is left to do appears here.

Items are numbered globally and ordered by priority: tier 1 gates submission, tier 5 is small touches.
Tags after each title:

- `[paper]` / `[code]` / `[both]`: where the work lives.
- `[scoping]`: needs a scope-out conversation before work starts.
- `[IRL]`: needs an in-person discussion (with Zhiyuan or the team).

Last refreshed: 2026-06-22 (items 17 and 20 closed).

---

## Tier 1: Submission-blocking decisions

1. **Cost model.** [paper, scoping]
   Obligation 2 in §1 promises improvement over SOTA, flagged in sketch.md as a legitimate open question.
   §5's primary metric (item 5), §7's evaluation (item 10), and the planner's tie-breaking among admissible label-set alignments (item 17 fallout) all depend on this decision.
   Candidate dimensions: transition duration in pop-ticks, packets dropped, packets reordered relative to SOTA, peak PE footprint during transition.

2. **§3.4.8 Graft soundness obligations.** [paper, IRL]
   The `[AM TODO]` at sketch.md line ~911 ("spell out the five obligations under the whole-tree restriction") is still open.
   Decide whole-tree-only vs. arbitrary sub-control plug, then write the five obligations.

3. **§3.5 / §6.3 substrate-atomicity prose.** [paper, IRL]
   AM-note to Zhiyuan; §3.5 should fold into §6.3 main text and then cite §6.3.
   `sketch.md` lines ~918 and ~1330.

4. **§6.2 port_root routing decision.** [paper]
   Routing question for `P` ports unresolved: assoc-as-gate vs. global class-to-root vs. class-to-port + port-to-root split.
   AM-note left in place; GH link still in the text (deferred 2026-06-15).
   Same site as item 23.

---

## Tier 2: §5 framework and §7-9 build-out

5. **§5 primary metric.** [paper]
   Pure confinement vs. cost-weighted; connects to item 1.

6. **§5 union-of-footprints bound vs. symmetric pol-level difference.** [paper]
   The compositional analog of §4 Safety: union of per-link footprints is the sequence's footprint, and confinement is monotone under union.

7. **§5 footprint must include `z` changes.** [paper]
   `Quiesce` and `Designate` touch ancestor `z`s even though `state` is preserved verbatim; the §5 footprint notion must reflect that.

8. **§5 confinement is per-sequence, not per-operator-request.** [paper]
   §4.4 follow-up queuing means the bound is over `(p1', p2)` and not `(p1', p3)`.

9. **§3.4 locality lemma / uniform footprint definition.** [paper]
   The per-production Preservation paragraphs implicitly give the locality lemma §5 needs, but each one names its "edit site" privately.
   A uniform `footprint(δ, C)` definition lifts this out (see item 6).

10. **§7 Evaluation buildout.** [paper]
    Gates on item 1.
    Test scenarios: §1's two running examples; `Add` cascade; `Replace` on occupied subtree; `PruneDownTo` from a wide tree.
    Compare against SOTA stop-the-world, `Replace([], p2)` fallback, and the planner's confined sequence.

11. **§8 Related Work clustering.** [paper]
    (a) PIFO substrates: Sivaraman PIFO, PIEO, SP-PIFO, vPIFO.
    (b) Formal scheduling models: FA.
    (c) Consistent network updates: Reitblatt et al., Mahajan-Wattenhofer (closest in spirit).
    (d) Programmable scheduling fabrics: Eiffel, Loom, Carousel, BMW, Gearbox.
    (e) P4 runtime updates.

12. **§9 Conclusion.** [paper]
    Restate the two contributions (link semantics; planner improvements over SOTA); call out open items (cost model, Graft, RR cursor under reorder).

13. **§5 worked examples and planner strengthening.** [both]
    `sketch.md` line ~1158: more examples; possible follow-on improvements to `rio/planner/planner.ml` driven by what the examples expose.

---

## Tier 3: Imperative mode

14. **PR 8: imperative-mode DSL surface.** [code, scoping]
    REPL session vs. .sched file; "present tree" display; surface syntax for guarded sequences; mode toggle.
    Closes the `project_imperative_mode_surface` memory.

15. **PR 9: imperative-mode pol-check.** [code]
    Gates on item 14.
    Two runtime checks on a user-supplied sequence: per-step validity (each `den(δ_i)` defined on `ip_{i-1}`) and intent verification (`ip_n =R p2`).
    Closes audit A6.

---

## Tier 4: Planner recognition tightenings (operationally high-leverage)

16. **(closed 2026-06-22)** Mixed Add and Retire at the same parent (`RR(A, B) -> RR(A, C, D)`): replaced `align_by_labels` with bidirectional `align_by_labels_bidir` returning `(matches, ps1_only, ps2_only)`; emit retires (descending ps1 idx) before adds (ascending ps2 idx) before match edits. Guard: `matches = []` falls back to give-up so the parent never empties during the sequence. Tests in `mixed_add_retire`.

17. **(closed; landed earlier in commit 88c5ffd)** Meta-only permutation under rank/weight sort: `compare_metaed_children`'s same-length branch carries a pre-pass that detects arm-content permutations and emits one `ChangeMeta` per slot whose meta moved.
    Test: `Strict with arms reordered` (`strict_AB` -> `strict_BA`).

18. **(closed 2026-06-22)** Constructor-only change (`RR(A, B) -> SP((A, 1), (B, 2))`) stays as a root-level `Replace`; no grammar addition. Item 24's "(b) extend the grammar" branch is closed by extension.

19. **Morphed sub-policy.** [code]
    `is_sub_policy` requires literal equality.
    A label-set search for a host subtree parallel to `align_by_labels`, then `Graft host` followed by bubbled inner-pair recursion.
    Symmetric for `PruneDownTo` with an inner morph; also covers `RR(A, B) -> RR(RR(A, X), B)`, which is §5.3's "future planner could do better here" running example.

20. **(closed 2026-06-22)** Same-length label-set fallback: both `compare_children` and `compare_metaed_children` now try `align_by_labels_bidir` in the same-length branch and use the bidir emit whenever some match has `i1 <> j2` (genuine cross-slot correspondence); otherwise the per-slot walk runs. Test: `WFQ same-length cross-slot morph` (`wfq_ABC` -> `wfq_Z_rrAY_rrCW`).

21. **`Pol.SP`'s `designated` bool ignored by `analyze`.** [code]
    Pattern matches `SP (pms1, _)` everywhere, so a `(true, false)` flip is invisible.
    Dead today (user-supplied policies don't carry `true`), but a latent blind spot once designation hits the wire format.

---

## Tier 5: Smaller paper open items

22. **§1 atomic-but-not-immediate example.** [paper]
    `sketch.md` line ~28: a nicer example than the current one.

23. **§6.2 push routing across `P` trees.** [paper]
    `sketch.md` line ~1238: how a push reaches the right port; related to item 4.

24. **§1 hard example: `Graft` cannot express it.** [paper]
    `project_paper_section_structure` memory.
    Open whether to (a) drop the hard example from §1, (b) extend the grammar (folds into item 18), or (c) document the limitation in §5.4.

25. **`⌊·⌋` definition broadening.** [paper]
    The realization equation in §3.3 needs `⌊·⌋` defined on more than freshly-compiled controls.
    Same scope as the §5 closure check (former review nit 21).
    `project_floor_brackets_definition` memory.

26. **§3 `den(δ)` precondition table.** [paper]
    `project_section_4_pending` memory: §4 is closed except for a TeX-time table of per-production preconditions.

27. **§4.1 no withdraw / abort mechanism.** [paper]
    `sketch.md` line ~1008: today's sequences have no escape hatch once committed; flag explicitly or design one.

28. **§4.3 declarative-mode differ description.** [paper]
    Former review nit 50 said "revisit when §5 lands"; §5 has now landed, so this is a re-read check that §4.3's stub is consistent with §5's case analysis.

---

End of worklist.
