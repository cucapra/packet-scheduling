# Full-transitive phase verification: FAIL

Start **240**, commit **278**, drain **535**, finish (double-buffer cleanup done) **587**. Staging took **38 cycles** across **27 accepted configuration instructions** and old-tree drain took **257 cycles**.

Packets: **26 before commit**, **26 during drain**, and **68 after drain**; **28 old packets** were pending at commit.


Install commit: 27 instructions / 41 cycles to publication. Cleanup commit: 30 instructions / 274 cycles to publication (including guard wait); final bank cleanup: 15 cycles.

| Fact / check | Expected | Observed | Result |
| --- | ---: | ---: | :---: |
| longer_queue_to_drain / serialized_configuration_rate | >= 26 | 38 | PASS |
| longer_queue_to_drain / staging_duration | >= 20 | 38 | PASS |
| longer_queue_to_drain / old_backlog_at_commit | >= 32 | 28 | FAIL |
| longer_queue_to_drain / drain_duration | >= 800 | 257 | FAIL |
| before_commit_old_policy / precommit_packet_count | >= 6 | 26 | PASS |
| before_commit_old_policy / new_packets_before_commit | 0 | 0 | PASS |
| before_commit_old_policy / rr_repetitions_before_commit | 0 | 0 | PASS |
| after_commit_drain_old_first / drain_phase_packet_count | >= 6 | 26 | PASS |
| after_commit_drain_old_first / new_packets_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_all_old | 0 | 0 | PASS |
| after_drain_new_policy / postdrain_packet_count | >= 6 | 68 | PASS |
| after_drain_new_policy / old_packets_after_drain | 0 | 2 | FAIL |
| after_drain_new_policy / sp_priority_reversals | 0 | 0 | PASS |
