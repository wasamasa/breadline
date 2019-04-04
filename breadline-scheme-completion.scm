(module breadline-scheme-completion
  (scheme-completer)

  (import scheme)
  (import (chicken base))
  (import (chicken string))
  (import (srfi 1))
  (import apropos)
  (import breadline)

  (include "breadline-scheme-completion-impl.scm"))
