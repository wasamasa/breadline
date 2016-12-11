(module readline
  (add-history! read-history! write-history! history-file
   completer-set!
   readline make-readline-port)

(import chicken scheme foreign)

(use ports extras srfi-4)

#>
#include "readline/readline.h"
#include "readline/history.h"
<#

;;; history

(define history-file (make-parameter #f))

(define add-history!
  (foreign-lambda void "add_history" (const nonnull-c-string)))

(define strerror (foreign-lambda c-string "strerror" int))

(define (history-error errno location)
  (make-composite-condition
   (make-property-condition 'exn
                            'location location
                            'message (strerror errno))
   (make-property-condition 'i/o)
   (make-property-condition 'file)))

;; TODO: figure out how history truncation works

(define (read-history! filename)
  (let ((ret ((foreign-lambda int "read_history" (const c-string)) filename)))
    (when (not (zero? ret))
      (abort (history-error ret 'read-history!)))))

(define (write-history! filename)
  (let ((ret ((foreign-lambda int "write_history" (const c-string)) filename)))
    (when (not (zero? ret))
      (abort (history-error ret 'write-history!)))))

;;; completion

#>
void *readline_completer_proc;

char *readline_completer(const char *prefix, int state) {
  C_word completer = CHICKEN_gc_root_ref(readline_completer_proc);
  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(prefix)));
  C_save(C_fix(state));
  C_save(C_string2(&a, prefix));
  C_word result = C_callback(completer, 2);
  if (result == C_SCHEME_FALSE) {
    return NULL;
  } else {
    char *src = C_c_string(result);
    size_t size = C_unfix(C_i_string_length(result));
    char *dest = malloc(size + 1);
    if (dest == NULL) {
      return NULL;
    }
    strncpy(dest, src, size);
    dest[size] = '\0';
    return dest;
  }
}
<#

((foreign-lambda* void ()
   "readline_completer_proc = CHICKEN_new_gc_root();"
   "rl_completion_entry_function = &readline_completer;"))

(define completer-set!
  (foreign-lambda* void ((scheme-object completer))
    "CHICKEN_gc_root_set(readline_completer_proc, completer);"))

;; HACK: readline's default completion uses file names from the
;; current directory, to disable it one can either bind TAB to a
;; different action or use a custom completion function constantly
;; returning NULL...

(define (dummy-completer _state _prefix) #f)
(completer-set! dummy-completer)

;;; REPL integration

;; TODO: use alternate interface, select(3) and thread-wait-for-i/o!
;; to react immediately to C-c

(define readline
  (foreign-safe-lambda c-string* "readline" (const nonnull-c-string)))

(define (make-readline-port #!optional prompt)
  (letrec ((buffer "")
           (position 0)
           (port #f)
           (read-char
            (lambda ()
              (cond
               ((not buffer)
                #!eof)
               ((char-ready?)
                (let ((char (string-ref buffer position)))
                  (set! position (add1 position))
                  char))
               (else
                (set! position 0)
                (let* ((prompt (or prompt ((repl-prompt))))
                       (result (readline prompt)))
                  (when result
                    (add-history! result))
                  (set! buffer result))
                (when buffer
                  (set! buffer (string-append buffer "\n")))
                (read-char)))))
           (char-ready? (lambda () (< position (string-length buffer))))
           (close (lambda () #f)))
    (when (history-file)
      (read-history! (history-file)))
    (set! port (make-input-port read-char char-ready? close))
    (set-port-name! port "(readline)")
    (when (history-file)
      (set-finalizer! port (lambda (p) (write-history! (history-file))))
      (on-exit (lambda () (write-history! (history-file)))))
    port))

)
