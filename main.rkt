#lang racket/base

(require racket/match
         racket/promise
         racket/string
         racket/unit)

; --------------------------------------------------------------------
; configuration
; --------------------------------------------------------------------

(struct wine-prefix-settings [profiles] #:prefab)
(struct wine-prefix-profile [name prefix tasks] #:prefab)
(struct wine-prefix-task [name kind payload] #:prefab)

(define wine-prefix-config-path
  (make-parameter (build-path (find-system-path 'home-dir) ".config" "wine-prefix.rktd")))

(define wine-prefix-config
  (make-parameter
   (delay
     (with-input-from-file (wine-prefix-config-path) wine-prefix-read-config))))

(define (wine-prefix-read-config [input-port (current-input-port)])
  (read input-port))

(define (wine-prefix-get-config)
  (force (wine-prefix-config)))

(define (wine-prefix-get-profile name)
  (for/first ([p (wine-prefix-settings-profiles (wine-prefix-get-config))]
              #:when (string-ci=? (wine-prefix-profile-name p) name))
    p))

; --------------------------------------------------------------------
; helpers
; --------------------------------------------------------------------

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

; --------------------------------------------------------------------
; verbs
; --------------------------------------------------------------------

(define (wine-prefix-run what [task-name "default"])
  (match-define (struct* wine-prefix-profile ([prefix prefix] [tasks tasks])) (wine-prefix-get-profile what))
  (match-define (struct* wine-prefix-task ([name name] [kind kind] [payload payload]))
    (for/first ([t tasks]
                #:when (string-ci=? (wine-prefix-task-name t) task-name))
      t))
  (with-environment-variables (["WINEPREFIX" prefix])
    (define wine-subprocess (apply simple-subprocess "wine" payload))
    (subprocess-wait wine-subprocess)
    (exit (subprocess-status wine-subprocess))))

(define (wine-prefix-kill what)
  (match-define (struct* wine-prefix-profile ([prefix prefix])) (wine-prefix-get-profile what))
  (with-environment-variables (["WINEPREFIX" prefix])
    (define wineserver-subprocess (simple-subprocess "wineserver" "-k"))
    (subprocess-wait wineserver-subprocess)
    (exit (subprocess-status wineserver-subprocess))))


(define (wine-prefix-help)
  (printf #<<EOF
wine-prefix usage:

    list                      -- list all configured profiles and tasks
    help                      -- this help message
    run     <profile> [name]  -- run task in profile's prefix with name, or task named `default'
    kill    <profile>         -- kill profile's prefix
    winecfg <profile>         -- Open winecfg in profile's prefix

EOF
          ))

(define (wine-prefix-list)
  (for ([profile (wine-prefix-settings-profiles (wine-prefix-get-config))])
    (match-define (struct* wine-prefix-profile ([name name] [prefix prefix] [tasks tasks]))
      profile)
    (printf "~a (~a)\n" name prefix)
    (for ([task tasks])
      (match-define (struct* wine-prefix-task ([name task-name] [kind kind] [payload payload]))
        task)
      (printf "\t~a\t~a\t~a\n" task-name kind payload))))

(define (wine-prefix-winecfg profile)
  (match-define (struct* wine-prefix-profile ([prefix prefix])) (wine-prefix-get-profile profile))
  (with-environment-variables (["WINEPREFIX" prefix])
    (define winecfg-subprocess (simple-subprocess "winecfg"))
    (subprocess-wait winecfg-subprocess)
    (exit (subprocess-status winecfg-subprocess))))

; --------------------------------------------------------------------
; entrypoint
; --------------------------------------------------------------------

(module+ main
  (require command-tree)
  (define wine-prefix-commands
    `([help ,wine-prefix-help]
      [run ,wine-prefix-run]
      [list ,wine-prefix-list]
      [kill ,wine-prefix-kill]
      [winecfg ,wine-prefix-winecfg]))
  (command-tree wine-prefix-commands (current-command-line-arguments)))
