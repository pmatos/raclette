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
;;
;; Standalone script to build racket itself.
;; There are a bunch of interesting options - you might want to take a look at the
;; project README.md or by running `racket build.rkt -h`.
;;
(require racket/contract
         racket/file
         racket/function
         racket/list
         racket/port
         racket/string
         racket/system)

(provide
 (contract-out
  [build-a-racket (->* (path-string? path-string?) (#:cs? boolean?
                                                    #:cc string?
                                                    #:cflags string?
                                                    #:cppflags string?
                                                    #:ldflags string?
                                                    #:j exact-positive-integer?
                                                    #:disable-docs? boolean?
                                                    #:pkgs (listof symbol?)
                                                    #:scope (or/c 'installation 'user))
                       void?)]
  [racket-version (-> path-string? version?)]))

;; ---------------------------------------------------------------------------------------------------

(define/contract (->string p)
  (-> path-string? string?)
  (if (string? p)
      p
      (path->string p)))

(define (configure source destination
                   #:mode [mode 'cs]
                   #:helper [helper #false]
                   #:cc [cc #false]
                   #:cflags [cflags #false]
                   #:cppflags [cppflags #false]
                   #:ldflags [ldflags #false]
                   #:disable-docs? [disable-docs? #false])
  (define config-args
    (string-append
     (if helper (format "--enable-racket=~a" helper) "")
     (if disable-docs? "--disable-docs " "")
     (cond
       [(eq? mode 'cs) "--csonly "]
       [(eq? mode 'cgc) "--enable-cgcdefault"]
       [else ""])
     (if cc (format "CC=~a " cc) "")
     (if cflags (format "CFLAGS=\"~a\" " cflags) "")
     (if cppflags (format "CPPFLAGS=\"~a\" " cppflags) "")
     (if ldflags (format "LDFLAGS=\"~a\" " ldflags) "")))
  
  (parameterize ([current-directory (build-path source "racket" "src")])
    (system (format "./configure --prefix=~a ~a" destination config-args))))

(define (build source
               #:j [j 1])
  (parameterize ([current-directory (build-path source "racket" "src")])
    (system (format "make -j~a" j))))

(define (clean source
               #:j [j 1])
  (parameterize ([current-directory (build-path source "racket" "src")])
    (system (format "make -j~a clean" j))))

(define (install source
                 #:j [j 1])
  (parameterize ([current-directory (build-path source "racket" "src")])
    (system (format "make -j~a install" j))))

(define/contract (set-catalog source destination)
  (-> path-string? path-string? void?)
  (define current-catalogs
    (string-split
     (with-output-to-string
       (thunk
        (void
         (system (format "~a/bin/raco pkg config catalogs"
                         (->string destination))))))))
  
  (void
   (system (format "~a/bin/racket -l- pkg/dirs-catalog --immediate ~a/catalog ~a/pkgs"
                        (->string destination)
                        (->string destination)
                        (->string source)))
   (system (format "~a/bin/raco pkg config --set catalogs ~a"
                   (->string destination)
                   (string-join (cons (format "~a/catalog" (->string destination))
                                      current-catalogs))))))
  
(define/contract (install-pkgs destination #:docs? [docs? #true] . pkgs)
  (->* (path-string?) (#:docs? boolean?) #:rest (listof symbol?)
       void)
  (define options
    (string-append
     (if docs? "" "-D"))) 
  (for ([pkg (in-list pkgs)])
    (system (format "~a/bin/raco pkg install --auto  ~a ~a"
                    (->string destination)
                    options
                    pkg))))

(struct version (cs? x y z w)
  #:transparent)

(define/contract (racket-version racket-bin)
  (-> path-string? version?)
  (define version
    (with-output-to-string
      (thunk
       (system (format "~a --version" racket-bin)))))

  (define maybe-cs?
    (regexp-match #px"cs" version))
  (define maybe-version
    (regexp-match #rx"v([0-9]+)\\.([0-9]+)(\\.([0-9]+))?(\\.([0-9]+))?" version))

  (version
   (and maybe-cs? #true)
   (second maybe-version)
   (third maybe-version)
   (if (fifth maybe-version) (fifth maybe-version) #false)
   (if (seventh maybe-version) (seventh maybe-version) #false)))

(define (set-scope destination scope)
  (system (format "~a/bin/raco pkg config --set default-scope \"~a\""
                  (->string destination)
                  scope)))

; Main entry function for building Racket
(define (build-a-racket source destination
                        #:cs? [cs? #true]
                        #:cc [cc #false]
                        #:cflags [cflags #false]
                        #:cppflags [cppflags #false]
                        #:ldflags [ldflags #false]
                        #:j [j 1]
                        #:disable-docs? [disable-docs? #false]
                        #:pkgs [pkgs '()]
                        #:scope [scope 'installation])
  
  (define cgc-destination (make-temporary-file "tmpracket~a" 'directory))
  (racket-build-chain source cgc-destination
                      #:mode 'cgc
                      #:cc cc
                      #:cflags cflags
                      #:cppflags cppflags
                      #:ldflags ldflags
                      #:j j
                      #:disable-docs? #true)

  (clean source #:j j)
  (racket-build-chain source destination
                      #:mode (if cs? 'cs '3m)
                      #:cc cc
                      #:cflags cflags
                      #:cppflags cppflags
                      #:ldflags ldflags
                      #:j j
                      #:pkgs pkgs
                      #:disable-docs? disable-docs?)
  (set-scope destination scope)

  (void))

(define (racket-build-chain source destination
                            #:mode mode
                            #:racket [helper #false]
                            #:cc [cc #false]
                            #:cflags [cflags #false]
                            #:cppflags [cppflags #false]
                            #:ldflags [ldflags #false]
                            #:j [j 1]
                            #:disable-docs? [disable-docs? #false]
                            #:pkgs [pkgs '()])

  ;; Configure
  (configure source destination
             #:mode mode
             #:helper helper
             #:cc cc
             #:cflags cflags
             #:cppflags cppflags
             #:ldflags ldflags
             #:disable-docs? disable-docs?)
             
  ;; Make
  (build source #:j j)
  
  ;; Make install
  (install source #:j j)
  
  ;; Set catalog
  (set-catalog source destination)
  
  ;; Install packages
  (apply install-pkgs destination #:docs? disable-docs? pkgs))
  
;; ---------------------------------------------------------------------------------------------------
;; Submodule main doing command line parsing and calling entry function build-a-racket
(module+ main
  (require racket/cmdline
           racket/future)

  ; Define options as parameters
  (define option-cores (make-parameter 1))
  (define option-source (make-parameter #false))
  (define option-destination (make-parameter #false))
  (define option-dry-run? (make-parameter #false))
  
  (define extra-configure-options
    (command-line
     #:program "build.rkt"

     #:once-each
     [("-j") cores "Number of cores to use or `all` (default: 1)"
      (cond
        [(string=? cores "all") (option-cores (processor-count))]
        [(and (string->number cores)
              (exact-positive-integer? (string->number cores)))
         (option-cores (string->number cores))]
        [else (error "argument to -j should be a positive integer or 'all', given:" cores)])]

     [("-s") source "Directory with Racket sources (default: checkout master)"
      (option-source source)]

     [("-d") destination "Destination directory"
      (option-destination destination)]

     [("-n") "Dry-run (do nothing but print plan)"
      (option-dry-run? #true)]
     
     #:args configure-options

     configure-options))

  (define source (resolve-path (path->complete-path (option-source))))
  (define destination (resolve-path (path->complete-path (option-destination))))
  
  (printf "Racket builder:~n")
  (printf "\tSource directory: ~a~n" source)
  (printf "\tDestination directory: ~a~n" destination)
  (printf "\tNumber of cores: ~a~n" (option-cores))
  (printf "\tExtra options: ~a~n" (if (null? extra-configure-options) "none" ""))
  (for ([opt (in-list extra-configure-options)]
        [n (in-range (length extra-configure-options))])
    (printf "\t\t~a. ~v~n" (+ 1 n) opt))

  (unless (option-dry-run?)
    (build-a-racket source destination
                    #:j (option-cores))))


;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/function)
  
  (run-tests (check #true)))

