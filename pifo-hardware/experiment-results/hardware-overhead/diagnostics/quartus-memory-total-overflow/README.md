# Quartus aggregate RAM total

The 1,024-ID read/copy synthesis completed successfully at 03:40:11 EDT on
2026-09-07. Quartus omitted both aggregate memory rows from its resource-usage
summary and printed `-2147483648` for the root hierarchy's block-memory count.
The experiment collector consequently rejected the missing field; this was
a reporting issue after successful synthesis.

Two independent sums from the completed report agree on **2,516,583,192 bits**:

- The `Implementation Bits` values of all 32 RAM instances.
- The `Block Memory Bits` of the root's disjoint direct children, with each
  child's descendants excluded from this sum.

The reader now uses that total only when the negative root sentinel, positive
instance counts, and independent child sum all agree. Raw report tables remain
unchanged. MLAB usage is unreported for this case and is omitted from comparisons
instead of being assigned zero. `validation.json` also checks that the other
13 completed Quartus runs retain their previous counts and that inconsistent
independent sums are rejected.

`collector-error-before-fix.json` preserves the original collector failure.
The updated readers are saved in `../../r4-replay/workflow/`; the earlier
`../../workflow/` snapshot remains the workflow that launched the references.
Use the current collection scripts to regenerate R1/R2/R4 from these completed
reports. Synthesis settings and RTL are unchanged by this correction.
