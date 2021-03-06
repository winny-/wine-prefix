#lang racket/base

(require racket/function
         racket/pretty
         racket/promise)

(provide (struct-out wine-prefix-settings)
         (struct-out wine-prefix-profile)
         (struct-out wine-prefix-task)
         wine-prefix-config-path
         wine-prefix-config
         wine-prefix-read-config
         wine-prefix-get-config
         wine-prefix-get-profile
         wine-prefix-get-task
         wine-prefix-backup-config
         wine-prefix-save-config)

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

(define (profile-failure-result what name)
  (raise-argument-error what "No such profile" name))
(define (wine-prefix-get-profile name [failure-result profile-failure-result])
  (define p (for/first ([p (wine-prefix-settings-profiles (wine-prefix-get-config))]
              #:when (string-ci=? (wine-prefix-profile-name p) name))
              p))
  (if p
      p
      (if (procedure? failure-result)
          (failure-result 'wine-prefix-get-profile name)
          failure-result)))

(define (task-failure-result what profile-name task-name)
  (raise-argument-error what "No such task" task-name))
(define (wine-prefix-get-task profile-name task-name [failure-result task-failure-result])
  (define p (wine-prefix-get-profile profile-name
                                     (cond
                                       [(equal? failure-result task-failure-result) profile-failure-result]
                                       [(procedure? failure-result) (curryr failure-result task-name)]
                                       [else failure-result])))
  (if (equal? p failure-result)
      p
      (let ([t (for/first ([t (wine-prefix-profile-tasks p)]
                     #:when (string-ci=? (wine-prefix-task-name t) task-name))
                 t)])
        (if t
            t
            (if (procedure? failure-result)
                (failure-result 'wine-prefix-get-task profile-name task-name)
                failure-result)))))

(define (wine-prefix-backup-config)
  (define config-path (path->string (wine-prefix-config-path)))
  (copy-file config-path (string-append config-path ".backup") #t))

(define (wine-prefix-save-config [cfg (force (wine-prefix-get-config))])
  (wine-prefix-backup-config)
  (with-output-to-file
    (wine-prefix-config-path)
    (thunk (pretty-write cfg))
    #:exists 'replace))
