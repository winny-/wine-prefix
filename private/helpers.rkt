#lang racket/base

(require racket/string)

(provide simple-subprocess
         with-environment-variables)

(define-syntax with-environment-variables
  (syntax-rules ()
    [(_ () body bodies ...)
     (begin body bodies ...)]
    [(_ ([a b] pairs ...) body bodies ...)
     (parameterize ([current-environment-variables (environment-variables-copy (current-environment-variables))])
       (putenv a b)
       (with-environment-variables (pairs ...)
         body bodies ...))]))

(define (simple-subprocess command-path-or-name . args)
  (define executable-path
    (if (string-contains? command-path-or-name "/")
        command-path-or-name
        (or (find-executable-path command-path-or-name)
            (error 'simple-subprocess "Could not locate ~a in PATH" command-path-or-name))))
  (define-values (the-subprocess stdout stdin stderr)
    (apply subprocess
           (current-output-port) (current-input-port) (current-error-port)
           executable-path
           args))
  the-subprocess)
