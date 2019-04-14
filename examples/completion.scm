(import scheme)
(import (chicken base))
(import (chicken string))
(import breadline)

(define completion-index 0)
(define completion-candidates #("foo" "bar" "baz" "💩")) ; <- utf-8 test

(define (test-completer prefix state)
  (when (zero? state)
    (set! completion-index 0))
  (let loop ()
    (if (< completion-index (vector-length completion-candidates))
        (let ((candidate (vector-ref completion-candidates completion-index)))
          (set! completion-index (add1 completion-index))
          (if (substring=? prefix candidate 0 0 (string-length prefix))
              candidate
              (loop)))
        #f)))

(completer-set! test-completer)
