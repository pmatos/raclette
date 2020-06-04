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
         racket/system)

(provide
 (contract-out
  [build-a-racket (->* (path-string? path-string?) (#:j exact-positive-integer?)
                       void?)]))

;; ---------------------------------------------------------------------------------------------------

; Main entry function for building Racket
(define (build-a-racket source destination
                        #:j [j 1])
  (define built?
    (parameterize ([current-directory source])
      (system (format "make unix-style CPUS=~a PREFIX=~a" j destination))))

  (unless built?
    (error "there was an error building racket, please refer to the logs")))

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
