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

;; Commentary

;; This module defines functions for interacting with the status bar.

;; Code

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
	"Set the status bar text to tree, which
must be pango markup text in SXML format."
	(set-title
	 (with-output-to-string
		 (lambda ()
			 (sxml->xml tree)))))

(define (title-format . tree)
	"Use format-to-sxml to convert tree to pango markup,
then apply set-title-sxml on the result."
	(set-title-sxml
	 (apply format-to-sxml tree)))


(define* (make-bar-updater separator bar-modules)
	"Create an object which periodically updates modules in the bar.

separator must be a string, which will be inserted between the outputs of bar modules.
bar-modules is a list, where each element is a plist describing a status bar module.

Supported plist properties:
#:fn - a function that should return an object that can be converted to pango markup using format-to-sxml.
#:name - a name for this bar module.
#:delay (optional) - must be a number specifying the number of seconds between updates of this module.
When #:delay is not specified, the module will never be automatically updated.

Example module:
(#:delay 1 #:fn (lambda () (fg \"red\" (format-time \"%r\"))) #:name \"clock\")

This module will display the current time in red color, and update every second.

Another example module:
(#:fn (let ((i 0))
  (lambda ()
    (set! i (1+ i))
    (number->string i)))
 #:name \"counter\")

This module will display a number that will be incremented every time the module is updated.

make-bar-updater returns two values:
1. A thread that will update the bar according to bar-modules
2. A list of bar modules.
This list will be identical to bar-modules, except that calling any of its functions will
force an update on the corresponding module in the bar.

You may use update-module to do this."

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
	"Return the plist with the given name from mods.
mods should be a plist returned by make-bar-updater."

	(find
	 (lambda (i)
		 (equal? (plist-get i #:name) name))
	 mods))

(define (update-module mods name)
	"Update the module with the given name in mods.
mods should be a plist returned by make-bar-updater."
	((plist-get (get-module mods name) #:fn)))
