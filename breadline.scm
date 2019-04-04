(module breadline
  (history-length history-file add-history! read-history! write-history!
   stifle-history! unstifle-history!
   completer-set! completer-word-break-characters-set!
   variable-bind! variable-value
   event-hook-set! pre-input-hook-set!
   insert-text delete-text stuff-char redisplay
   basic-quote-characters-set! paren-blink-timeout-set!
   readline make-readline-port)

  (import scheme)
  (import (chicken base))
  (import (chicken condition))
  (import (chicken file))
  (import (chicken foreign))
  (import (chicken format))
  (import (chicken gc))
  (import (chicken port))
  (import (chicken repl))

  (include "breadline-impl.scm"))
