(module readline
  (history-file add-history! read-history! write-history!
   stifle-history! unstifle-history!
   completer-set! completer-word-break-characters-set!
   variable-bind! variable-value
   basic-quote-characters-set! paren-blink-timeout-set!
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

(define (read-history! filename)
  (let ((ret ((foreign-lambda int "read_history" (const c-string)) filename)))
    (when (not (zero? ret))
      (abort (history-error ret 'read-history!)))))

(define (write-history! filename)
  (let ((ret ((foreign-lambda int "write_history" (const c-string)) filename)))
    (when (not (zero? ret))
      (abort (history-error ret 'write-history!)))))

(define stifle-history!
  (foreign-lambda void "stifle_history" int))

(define unstifle-history!
  (foreign-lambda int "unstifle_history"))

;;; completion

#>
void *readline_completer_proc;

char *copy_scheme_string(C_word string) {
  char *src = C_c_string(string);
  size_t size = C_unfix(C_i_string_length(string));
  char *dest = malloc(size + 1);
  if (dest == NULL) {
    return NULL;
  }
  strncpy(dest, src, size);
  dest[size] = '\0';
  return dest;
}

char *readline_completer(const char *prefix, int state) {
  C_word completer = CHICKEN_gc_root_ref(readline_completer_proc);
  int size = C_SIZEOF_STRING(strlen(prefix));
  C_word *a = C_alloc(size);
  // TODO: ensure this fixes stuff
  C_callback_adjust_stack(a, size);
  C_save(C_fix(state));
  C_save(C_string2(&a, (char *) prefix));
  C_word result = C_callback(completer, 2);
  if (result == C_SCHEME_FALSE) {
    return NULL;
  } else {
    return copy_scheme_string(result);
  }
}
<#

(foreign-code
 "readline_completer_proc = CHICKEN_new_gc_root();"
 "rl_completion_entry_function = &readline_completer;")

(define completer-set!
  (foreign-lambda* void ((scheme-object completer))
    "CHICKEN_gc_root_set(readline_completer_proc, completer);"))

;; HACK: readline's default completion uses file names from the
;; current directory, to disable it one can either bind TAB to a
;; different action or use a custom completion function constantly
;; returning NULL...

(define (dummy-completer _state _prefix) #f)
(completer-set! dummy-completer)

(define completer-word-break-characters-set!
  (foreign-lambda* void ((scheme-object string))
    "char *chars = copy_scheme_string(string);"
    "if (chars) rl_completer_word_break_characters = chars;"))

;;; misc

(define variable-bind!
  ;; NOTE: this seems to always return zero
  (foreign-lambda int "rl_variable_bind" (const nonnull-c-string) (const nonnull-c-string)))

(define variable-value
  (foreign-lambda c-string "rl_variable_value" (const nonnull-c-string)))

(define basic-quote-characters-set!
  (foreign-lambda* void ((scheme-object string))
    "char *chars = copy_scheme_string(string);"
    "if (chars) rl_basic_quote_characters = chars;"))

(define paren-blink-timeout-set!
  (foreign-lambda int "rl_set_paren_blink_timeout" int))

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
    (when (and (history-file) (file-exists? (history-file)))
      (read-history! (history-file)))
    (set! port (make-input-port read-char char-ready? close))
    (set-port-name! port "(readline)")
    (when (history-file)
      (set-finalizer! port (lambda (p) (write-history! (history-file))))
      (on-exit (lambda () (write-history! (history-file)))))
    port))

)
