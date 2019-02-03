#lang racket/base

(require racket/function
         racket/list
         racket/match
         racket/pretty
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

(define (wine-prefix-backup-config)
  (define config-path (path->string (wine-prefix-config-path)))
  (copy-file config-path (string-append config-path ".backup") #t))

(define (wine-prefix-save-config [cfg (force (wine-prefix-get-config))])
  (wine-prefix-backup-config)
  (with-output-to-file
    (wine-prefix-config-path)
    (thunk (pretty-write cfg))
    #:exists 'replace))

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

(define (wine-prefix-exec profile command . args)
  (match-define (struct* wine-prefix-profile ([prefix prefix])) (wine-prefix-get-profile profile))
  (with-environment-variables (["WINEPREFIX" prefix])
    (define the-subprocess (apply simple-subprocess command args))
    (subprocess-wait the-subprocess)
    (exit (subprocess-status the-subprocess))))

(define (wine-prefix-run what [task-name "default"])
  (match-define (and profile (struct* wine-prefix-profile ([tasks tasks])))
    (wine-prefix-get-profile what))
  (match-define (struct* wine-prefix-task ([name name] [kind kind] [payload payload]))
    (for/first ([t tasks]
                #:when (string-ci=? (wine-prefix-task-name t) task-name))
      t))
  (apply wine-prefix-exec what "wine" payload))

(define (wine-prefix-kill profile)
  (wine-prefix-exec profile "wineserver" "-k"))

(define (wine-prefix-winecfg profile)
  (wine-prefix-exec profile "winecfg"))

(define (wine-prefix-shell profile [shell (getenv "SHELL")])
  (match-define (struct* wine-prefix-profile ([prefix prefix])) (wine-prefix-get-profile profile))
  (parameterize ([current-directory prefix])
    (wine-prefix-exec profile shell)))

(define (wine-prefix-help)
  (printf #<<EOF
wine-prefix usage:

    list                                 -- list all configured profiles and
                                            tasks
    help                                 -- this help message
    run     <profile> [name]             -- run task in profile's prefix with
                                            name, or task named `default'
    kill    <profile>                    -- kill profile's prefix
    winecfg <profile>                    -- open winecfg in profile's prefix
    exec    <profile> <prog> [args ...]  -- run prog with args in profile's
                                            prefix
    shell   <profile> [shell]            -- chdir into profile's prefix and
                                            run shell, or $SHELL
    add profile <profile> <prefix>       -- Add profile with prefix to config
    add task <profile> <name> <kind> <args ...>  -- Add task to profile,
                                                    `kind` should be program

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

(define (wine-prefix-add what . args)
  (match what
    ["profile" (apply wine-prefix-add-profile args)]
    ["task" (apply wine-prefix-add-task args)]
    [a
     (fprintf (current-error-port) "Invalid object `~a'.\n" a)
     (wine-prefix-help)
     (exit 1)]))

(define (wine-prefix-add-profile name prefix)
  (when (wine-prefix-get-profile name)
    (fprintf (current-error-port) "Profile with `~a' already exists.\n" name)
    (exit 1))
  (wine-prefix-save-config (wine-prefix-settings (append (wine-prefix-settings-profiles (wine-prefix-get-config))
                                                         (list (wine-prefix-profile name prefix empty))))))

(define (wine-prefix-add-task profile name kind . payload)
  (define new-task (wine-prefix-task name (string->symbol kind) payload))
  (wine-prefix-save-config
   (wine-prefix-settings
    (for/list ([p (wine-prefix-settings-profiles (wine-prefix-get-config))])
      (match-define (struct* wine-prefix-profile ([name the-profile-name] [tasks the-tasks])) p)
      (if (string-ci=? the-profile-name profile)
          (struct-copy wine-prefix-profile p [tasks (append the-tasks (list new-task))])
          p)))))

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
      [winecfg ,wine-prefix-winecfg]
      [exec ,wine-prefix-exec]
      [shell ,wine-prefix-shell]
      [add ,wine-prefix-add]))
  (command-tree wine-prefix-commands (current-command-line-arguments)))
