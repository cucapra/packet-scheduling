# Full-transitive phase verification: FAIL

Start **600**, commit **633**, drain **1924**, finish (double-buffer cleanup done) **1972**. Staging took **33 cycles** across **25 accepted configuration instructions** and old-tree drain took **1291 cycles**.

Packets: **101 before commit**, **146 during drain**, and **233 after drain**; **148 old packets** were pending at commit.

| Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| C1: policy-change | 600 | 633 | 816 | 834 | 25 | 216 | 18 |
| C2: policy-change-cleanup | 834 | 852 | 1945 | 1955 | 17 | 1111 | 10 |
| C3: policy-change-reclaim | 1955 | 1964 | 1967 | 1972 | 9 | 12 | 5 |

| Fact / check | Expected | Observed | Result |
| --- | ---: | ---: | :---: |
| longer_queue_to_drain / serialized_configuration_rate | >= 24 | 33 | PASS |
| longer_queue_to_drain / staging_duration | >= 10 | 33 | PASS |
| longer_queue_to_drain / old_backlog_at_commit | >= 64 | 148 | PASS |
| longer_queue_to_drain / drain_duration | >= 1000 | 1291 | PASS |
| longer_queue_to_drain / prefill_matches_old_backlog | 148 | 148 | PASS |
| longer_queue_to_drain / admissions_while_hardware_stopped | 0 | 0 | PASS |
| before_commit_old_policy / precommit_packet_count | >= 6 | 101 | PASS |
| before_commit_old_policy / new_packets_before_commit | 0 | 0 | PASS |
| before_commit_old_policy / rr_repetitions_before_commit | 0 | 0 | PASS |
| after_commit_drain_old_first / drain_phase_packet_count | >= 6 | 146 | PASS |
| after_commit_drain_old_first / new_packets_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_during_drain | 0 | 0 | PASS |
| after_commit_drain_old_first / rr_repetitions_all_old | 0 | 0 | PASS |
| after_drain_new_policy / postdrain_packet_count | >= 6 | 233 | PASS |
| after_drain_new_policy / old_packets_after_drain | 0 | 2 | FAIL |
| after_drain_new_policy / sp_priority_reversals | 0 | 0 | PASS |
