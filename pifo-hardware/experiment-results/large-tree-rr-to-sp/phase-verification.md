# Full-transitive phase verification: PASS

Start **240**, commit **268**, drain **1475**, finish **8464**. Staging took **28 cycles** across **28 accepted configuration instructions** and old-tree drain took **1207 cycles**.

Packets: **9 before commit**, **45 during drain**, and **66 after drain**; **45 old packets** were pending at commit.

| Fact / check | Expected | Observed | Result |
| --- | ---: | ---: | :---: |
| longer_queue_to_drain / serialized_configuration_rate | >= 28 | 28 | PASS |
| longer_queue_to_drain / staging_duration | >= 20 | 28 | PASS |
| longer_queue_to_drain / old_backlog_at_commit | >= 32 | 45 | PASS |
| longer_queue_to_drain / drain_duration | >= 800 | 1207 | PASS |
| before_commit_old_policy / precommit_packet_count | >= 6 | 9 | PASS |
| before_commit_old_policy / new_packets_before_commit | 0 | 0 | PASS |
| before_commit_old_policy / rr_repetitions_before_commit | 0 | 0 | PASS |
| after_commit_drain_old_first / drain_phase_packet_count | >= 6 | 45 | PASS |
| after_commit_drain_old_first / new_packets_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_all_old | 0 | 0 | PASS |
| after_drain_new_policy / postdrain_packet_count | >= 6 | 66 | PASS |
| after_drain_new_policy / old_packets_after_drain | 0 | 0 | PASS |
| after_drain_new_policy / sp_priority_reversals | 0 | 0 | PASS |
