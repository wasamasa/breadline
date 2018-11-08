(module breadline-scheme-completion
  (scheme-completer)

(import scheme)
(import (chicken base))
(import (chicken string))
(import (srfi 1))
(import apropos)
(import breadline)

(define (apropos-completions prefix)
  (let ((candidates (apropos-list `(: bos ,prefix) #:macros? #t)))
    (remove
     (lambda (candidate) (substring-index "#" candidate))
     (map symbol->string candidates))))

(define (current-environment-completions prefix)
  (let ((size (string-length prefix)))
    (filter
     (lambda (candidate) (substring=? prefix candidate 0 0 size))
     (map (o symbol->string car) (##sys#current-environment)))))

(define (make-completer #!rest procs)
  (let ((completions #())
        (index 0))
    (lambda (prefix state)
      (when (zero? state)
        (let ((candidates (append-map (lambda (proc) (proc prefix)) procs)))
          (set! completions (list->vector candidates)))
        (set! index 0))
      (if (< index (vector-length completions))
          (let ((completion (vector-ref completions index)))
            (set! index (add1 index))
            completion)
          #f))))

(define scheme-completer
  (make-completer apropos-completions current-environment-completions))

)
