#lang racket/base
;;
;; Copyright 2020 Paulo Matos
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; ---------------------------------------------------------------------------------------------------

(require "private/build.rkt"
         db
         deta
         net/git-checkout
         racket/cmdline
         racket/file
         racket/string
         (only-in racket/list first rest))

;; ---------------------------------------------------------------------------------------------------

(define raclette-dir
  (build-path (find-system-path 'home-dir) ".raclette"))

(define db-file
  (build-path raclette-dir "raclette.db"))

;; Information on structures
(define-schema rkt
  ([id id/f #:primary-key #:auto-increment]
   [version string/f #:contract non-empty-string?]
   [path string/f #:contract path-string?]))

(define (handle-init args)

  (unless (null? args)
    (fprintf (current-error-port)
             "expected 0 arguments to `init`, got ~a: ~a~n" (length args) args)
    (exit 1))
  
  ;; create .raclette in $HOME if doesn't exist yet
  (unless (directory-exists? raclette-dir)
    (make-directory raclette-dir))
    
  (define conn
    (sqlite3-connect #:database db-file
                     #:mode 'create))

  (create-table! conn 'rkt)
  
  (disconnect conn))

(define (handle-install args)
  (printf "handle-install args: ~v~n" args)
  (define version (make-parameter #f))
  (command-line
   #:argv args
   #:once-each
   [("-v" "--version") ver "Racket version to install"
    (version ver)])

  ;; Checkout racket
  (define tmpdir (make-temporary-file "raclette~a" 'directory))
  (define racketdir (build-path tmpdir "racket"))
  (define checkout-id
    (git-checkout "github.com" "racket/racket"
                  #:depth 1
                  #:dest-dir racketdir))
  (printf "checkout id ~v~n" checkout-id)
  
  ;; Build a new racket with installation scope
  (define destination (build-path raclette-dir "installs" checkout-id))
  (build-a-racket racketdir destination)

  ;; Remove temporary directory
  (delete-directory/files tmpdir #:must-exist #t)
  
  ;; register new racket
  (define installation
    (make-rkt
     #:version checkout-id
     #:path destination))

  (define conn (sqlite3-connect #:database db-file))
  (insert-one! conn installation)
  (disconnect conn))

(define (handle-remove args)
  ;; Remove racket installation

  ;; deregister racket
  (void))

(define (handle-show args)
  ;; List all installed rackets
  (void))

(define (handle-set args)
  ;; Set default racket
  (void))

;; Available sub-commands
(define command-handlers
  `((init    . ,handle-init)
    (install . ,handle-install)
    (remove  . ,handle-remove)
    (show    . ,handle-show)
    (set     . ,handle-set)))

(define (usage)
  (fprintf (current-error-port) "raclette ...~n"))

(define (handle-args args)
  
  (when (null? args)
    (usage)
    (exit 1))
  
  (define cmd (assq (string->symbol (first args)) command-handlers))
  (unless cmd
    (fprintf (current-error-port) "invalid command: ~a~n" cmd)
    (usage)
    (exit))

  ;; calling command handler
  ((cdr cmd) (rest args)))

(module+ main

  (require racket/cmdline)

  ;; Only supported on unix and macos
  (unless (memq (system-type 'os) '(unix macos))
    (fprintf (current-error-port)
             "Oh dear... unsupported OS!~n")
    (exit 1))
               
  ;; setup database file
  (unless (sqlite3-available?)
    (fprintf (current-error-port)
             "Houston, we have a problem - no sqlite3 available~n")
    (exit 1))
  
  (command-line
   #:program "raclette"
   #:once-each
   [("-?") "Show help"
    (usage)
    (exit)]
   #:args args
   (handle-args args)))
  
