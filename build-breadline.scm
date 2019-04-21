(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken format))
(import (chicken io))
(import (chicken process))
(import (chicken process-context))
(import (chicken string))

(define (library-flags var command)
  (or (get-environment-variable var)
      (let ((exit (system (string-append command " > /dev/null"))))
        (if (zero? exit)
            (let ((output (with-input-from-pipe command (cut read-string #f))))
              (when (eof-object? output)
                (error (format "Command didn't produce any output: ~a" command)))
              output)
            (error (format "Command failed with exit code ~s, set $~a" exit var))))))

(define csc (get-environment-variable "CHICKEN_CSC"))
(define readline-cflags (library-flags "READLINE_CFLAGS" "(pkg-config --cflags readline || echo '')"))
(define readline-ldlibs (library-flags "READLINE_LDLIBS" "(pkg-config --libs readline || echo '-lreadline -lncurses')"))

(define args (list csc readline-cflags readline-ldlibs))
(define cmdline
  (string-append (apply format "~a -C ~a -L ~a " (map qs args))
                 (string-intersperse (map qs (command-line-arguments)) " ")))

(system cmdline)
