#lang racket/base

(require racket/list
         racket/match)

(require "configuration.rkt"
         "helpers.rkt")

(provide wine-prefix-exec
         wine-prefix-run
         wine-prefix-kill
         wine-prefix-winecfg
         wine-prefix-shell
         wine-prefix-help
         wine-prefix-list
         wine-prefix-add-profile
         wine-prefix-add-task
         wine-prefix-add
         wine-prefix-remove
         wine-prefix-remove-task
         wine-prefix-remove-profile)

(define (wine-prefix-exec profile command . args)
  (match-define (struct* wine-prefix-profile ([prefix prefix])) (wine-prefix-get-profile profile))
  (with-environment-variables (["WINEPREFIX" prefix])
    (define the-subprocess (apply simple-subprocess command args))
    (subprocess-wait the-subprocess)
    (exit (subprocess-status the-subprocess))))

(define (wine-prefix-run what [task-name "default"])
  (match-define (struct* wine-prefix-task ([name name] [kind kind] [payload payload]))
    (wine-prefix-get-task what task-name))
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
    remove profile <profile>             -- Remove profile.
    remove task <profile> <task>         -- Remove task on profile.

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

(define (wine-prefix-remove what . args)
  (match what
    ["profile" (apply wine-prefix-remove-profile args)]
    ["task" (apply wine-prefix-remove-task args)]
    [a
     (fprintf (current-error-port) "Invalid object `~a'.\n" a)
     (wine-prefix-help)
     (exit 1)]))

(define (wine-prefix-remove-profile name)
  (unless (wine-prefix-get-profile name)
    (fprintf (current-error-port) "Profile `~a' does not exist.\n" name)
    (exit 1))
  (define config (wine-prefix-get-config))
  (define new-profiles (filter (λ (p) (not (string-ci=? (wine-prefix-profile-name p) name)))
                               (wine-prefix-settings-profiles config)))
  (wine-prefix-save-config (struct-copy wine-prefix-settings config [profiles new-profiles])))

(define (wine-prefix-remove-task profile task)
  (unless (wine-prefix-get-task profile task)
    (fprintf (current-error-port "Task `~a' in profile `~a' does not exist.\n" task profile))
    (exit 1))
  (define config (wine-prefix-get-config))
  (define new-profiles
    (for/list ([p (wine-prefix-settings-profiles config)])
      (if (string-ci=? (wine-prefix-profile-name p) profile)
          (struct-copy wine-prefix-profile p [tasks (for/list ([t (wine-prefix-profile-tasks p)]
                                                               #:unless (string-ci=? (wine-prefix-task-name t) task))
                                                      t)])
          p)))
  (wine-prefix-save-config (struct-copy wine-prefix-settings config [profiles new-profiles])))
