#lang racket/base

(require command-tree)

(require "verbs.rkt")

(provide wine-prefix-commands
         main)

(define wine-prefix-commands
  `([help ,wine-prefix-help]
    [run ,wine-prefix-run]
    [list ,wine-prefix-list]
    [kill ,wine-prefix-kill]
    [winecfg ,wine-prefix-winecfg]
    [exec ,wine-prefix-exec]
    [shell ,wine-prefix-shell]
    [add ,wine-prefix-add]
    [remove ,wine-prefix-remove]))

(define (main [arguments (current-command-line-arguments)])
  (command-tree wine-prefix-commands arguments))
