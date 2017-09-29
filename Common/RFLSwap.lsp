;
;
;     Program written by Robert Livingston, 2017-09-29
;
;     RFL:SWAP swaps varriables A and B
;
;
(defun RFL:SWAP (A B / TMP)
 (setq TMP A A B B TMP)
 T
)