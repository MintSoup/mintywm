;; This file is part of mintywm.
;;
;; mintywm is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; mintywm is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; mintywm. If not, see <https://www.gnu.org/licenses/>.
(define-module (mintywm bar)
	#:use-module (srfi srfi-1)
	#:use-module (srfi srfi-17)
	#:use-module (srfi srfi-18)

	#:use-module (mintywm core)
	#:use-module (mintywm utils)
	#:use-module (mintywm markup)
	#:use-module (sxml simple)

	#:use-module (al plists)

	#:export
	(set-title-sxml
	 title-format
	 make-bar-updater
	 get-module
	 update-module))

(define (set-title-sxml tree)
	(set-title
	 (with-output-to-string
		 (lambda ()
			 (sxml->xml tree)))))

(define (title-format . tree)
	(set-title-sxml
	 (apply format-to-sxml tree)))


(define* (make-bar-updater separator bar-modules)
	(define bar-sxml
		(fold
		 (lambda (mod prev)
			 (cons separator (cons ((plist-get mod #:fn)) prev)))
		 '() bar-modules))

	(define (push)
		(apply title-format (cdr bar-sxml)))

	(define modules
		(map
		 (lambda (mod n)
			 (plist-put (list-copy mod) #:fn
						(lambda ()
							(set! (car (drop bar-sxml (1+ (* 2 n))))
								  ((plist-get mod #:fn)))
							(push))))
		 bar-modules
		 (iota (length bar-modules))))

	(values
	 (make-thread
	  (lambda ()
		  (make-scheduler
		   (map
			(lambda (mod)
				(cons (plist-get mod #:delay)
					  (plist-get mod #:fn)))
			(filter
			 (lambda (mod)
				 (plist-get mod #:delay)) modules)))))
	 modules))

(define (get-module mods name)
	(find
	 (lambda (i)
		 (equal? (plist-get i #:name) name))
	 mods))

(define (update-module mods name)
	((plist-get (get-module mods name) #:fn)))
