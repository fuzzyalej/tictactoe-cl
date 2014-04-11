(defvar *player* 1)

(defvar *computer* 10)

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((eql v 1) "O")
        ((eql v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defvar *triplets*
  '((1 2 3) (4 5 6) (7 8 9) ; Horizontal
    (1 4 7) (2 5 8) (3 6 9) ; Vertical
    (1 5 9) (3 5 7)))       ; Diagonal

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *player*) sums)
        (member (* 3 *computer*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos(pick-random-empty-position board))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                   #'(lambda (trip)
                       (equal (sum-triplet board trip) target-sum))
                   *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "Random move"))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "Try to make three in a row"))))

(defun block-player-win (board)
  (let ((pos (win-or-block board (* 2 *player*))))
    (and pos (list pos "Blocking player win"))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-player-win board)
      (random-move-strategy board)))

(defun computer-move (board)
  "Placeholder"
  board)

(defun player-move (board)
  (format t "~&Your move: ")
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *player* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~S" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (player-move new-board)))))

(defun play ()
  (if (y-or-n-p "Would you like to play first? ")
    (player-move (make-board))
    (computer-move (make-board))))

(play)
