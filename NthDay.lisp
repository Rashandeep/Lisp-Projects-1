; Short program to calculate the number of gifts
; received by the end of the nth day of Christmas
; following the pattern from the song.

; How many gifts received ON day n
(defun num-gifts-day-n (n)
  (if (eq n 1)
    1
    (+ n (num-gifts-day-n (- n 1)))))

; How many gifts received through days
; 1 .. n
(defun num-gifts-n-days (n)
  (if (eq n 1)
    1
    (+ (num-gifts-day-n n)
      (num-gifts-n-days (- n 1)))))

; Print loop to demonstrate the above routines
(defun n-days-of-xmas (n)
  (do ((d 1 (+ d 1)))
      ((> d n) 'done)
      (format t "After ~A days of Christmas, he had received ~A gifts ~%" d (num-gifts-n-days d))))
