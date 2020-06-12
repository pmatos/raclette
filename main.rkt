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
         gregor
         net/git-checkout
         racket/cmdline
         racket/file
         (only-in racket/future processor-count)
         racket/string
         racket/system
         (only-in racket/list first rest))

;; ---------------------------------------------------------------------------------------------------

(define raclette-dir
  (build-path (find-system-path 'home-dir) ".raclette"))
(define default-link
  (build-path raclette-dir "default"))
(define db-file
  (build-path raclette-dir "raclette.db"))

;; Information on structures
(define-schema rkt
  ([id           id/f          #:primary-key #:auto-increment]
   [install-date datetime-tz/f #:contract moment-provider?]
   [version      string/f      #:contract string?]
   [ref          string/f      #:contract non-empty-string?]
   [path         string/f      #:contract path-string?]))

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
  (define maybe-versions
    (command-line #:program "raclette install"
     #:argv args
     #:args (v . vlst)
     (cons v vlst)))
  
  (for ([v (in-list maybe-versions)])
    (install v)))

(define (install-destination ref)
  (-> string? path-string?)
  (build-path raclette-dir "installs" checkout-id))
  
(define (install ref)
  
  ;; Checkout racket
  (define tmpdir (make-temporary-file "raclette~a" 'directory))
  (define racketdir (build-path tmpdir "racket"))
  (define checkout-id
    (git-checkout "github.com" "racket/racket"
                  #:ref ref
                  #:depth 1
                  #:dest-dir racketdir))
  (printf "checkout id ~v~n" checkout-id)
  
  ;; Build a new racket with installation scope
  (define destination (install-destination ref))
  (build-a-racket racketdir destination
                  #:cs? #false
                  #:cflags "-flto -O3 -march=native"
                  #:ldflags "-flto -O3 -march=native"
                  #:j (processor-count)
                  #:scope 'installation
                  #:pkgs '(main-distribution))

  ;; Force package scope installation
  (system (format "~a/bin/raco pkg config --set default-scope \"installation\"" (path->string destination)))
  
  ;; Remove temporary directory
  (delete-directory/files tmpdir #:must-exist? #t)
  
  ;; register new racket
  (define installation
    (make-rkt
     #:install-date (now/moment)
     #:version ref
     #:ref checkout-id
     #:path (path->string destination)))

  ;; Set it as default if there's none as default
  (unless (get-default)
    (set-default/ref checkout-id))
  
  (define conn (sqlite3-connect #:database db-file))
  (insert-one! conn installation)
  (disconnect conn))


(define/contract (get-default)
  (-> (or/c #false rkt?))

  (cond
    [(link-exists? default-link)
     (define default (resolve-path default-link))
     (define ref (path->string (last (explode-path default))))
     (get-rkt/ref ref)]
    [else #false]))

(define/contract (get-rkt/ref ref)
  (-> string (or/c #false rkt?))

  (with-db-conn 'read-only  
    ;; There can be only one...
    (lookup conn
            (~> (from rkt #:as r)
                (where (= rkt.ref ref))))))

(define/contract (get-rkt version)
  (-> string (or/c #false rkt?))

  (with-db-conn 'read-only  
    ;; There can be only one...
    (lookup conn
            (~> (from rkt #:as r)
                (where (= rkt.version version))))))


(define/contract (set-default/ref ref)
  (-> string? void)

  (when (get-default)
    (delete-file default-link))

  (define to-path (install-destination ref))
  
  (void
   (make-file-or-directory-link to-path default-link)))

;; TODO: We need a #:as ... so that the conn variable is accessible outside
;;       the macro. This is not working until then!!!
(define-syntax-rule (with-db-conn mode e ...)
  (begin
    (define conn #false)
    (dynamic-wind
      (thunk
       (set! conn (sqlite3-connect #:database db-file
                                   #:mode mode)))
      (thunk e ...)

      (thunk (when conn (disconnect conn))))))

(define (handle-remove args)
  ;; Remove racket installation
  (define ref? #false)
  (define v
    (command-line
     #:program "raclette remove"
     #:once-each
     [("-r" "--reference") "Treat argument as reference"
      (set! ref? #true)]
     #:args (v)
     v))
  
  (define rkt
    (with-db-conn 'read-only
      (cond
        [ref? (get-rkt/ref v)]
        [else (get-rkt v)])))

  (delete-directory/files (rkt-path rkt))
  
  ;; deregister racket
  (delete! conn rkt))

(define (show-rkt-1line arkt)
  (printf "~t* ~a~n" (rkt-version arkt)))
  
(define (handle-show args)
  ;; List all installed rackets
  (define conn (sqlite3-connect #:database db-file
                                #:mode 'read-only))

  (printf "Installed Racket:~n")
  (for ([v (in-entities conn (from rkt #:as r))])
    (show-rkt-1line v))
  
  (disconnect conn))

(define (handle-set args)
  ;; Set default racket
  (void))

(define (handle-get args)
  ;; Get default racket
  (void))

;; Available sub-commands
(define command-handlers
  `((init    . ,handle-init)
    (install . ,handle-install)
    (remove  . ,handle-remove)
    (show    . ,handle-show)
    (set     . ,handle-set)
    (get     . ,handle-get)))

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
  
