(define (problem bw_break1)
  (:domain bw_break)
  (:objects A B C D)
  (:init (on-table A) (clear A)
	 (on-table B) (clear B)
	 (on-table C) (clear C)
	 (on-table D) (clear D)
	 )
  (:goal (and (on A B) (on B C) (on C D)))
)
