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

;; This module is used to convert our custom SXML based markup tree format
;; into pango markup SXML, which can later be converted to pango markup using
;; sxml->xml.

;; Code

(define-module (mintywm markup)
	#:use-module (ice-9 match))

(define-public (format-to-sxml . tree)
	"Convert our custom markup format to pango markup SXML.

Example usage:

Bold red text \"hello\":
    (format-to-sxml '(b (fg \"red\" \"hello\")))

Underlined text:
    (format-to-sxml '(u \"hello\"))

Italic text with #51afef as its fg and black as its bg color:
    (format-to-sxml '(it (fg \"#51afef\" (bg \"black\" hello))))

For more examples, check out the source code.
"
	(match tree
		[(('b rest ...))
		 `(span (@ (weight "bold")) ,(apply format-to-sxml rest))]

		[(('it rest ...))
		 `(span (@ (style "italic")) ,(apply format-to-sxml rest))]

		[(('size siz rest ...))
		 `(span (@ (size ,(* siz 1024))) ,(apply format-to-sxml rest))]

		[(('face fac rest ...))
		 `(span (@ (face ,fac)) ,(apply format-to-sxml rest))]

		[(('fg col rest ...))
		 `(span (@ (fgcolor ,col)) ,(apply format-to-sxml rest))]

		[(('bg col rest ...))
		 `(span (@ (bgcolor ,col)) ,(apply format-to-sxml rest))]

		[(('u rest ...))
		 `(span (@ (underline single)) ,(apply format-to-sxml rest))]

		[(('s rest ...))
		 `(span (@ (strikethrough true)) ,(apply format-to-sxml rest))]

		[(first)
		 (if (string? first)
			 first
			 (error "Only strings are allowed as primitives."))]

		[(first rest ...)
		 `(,(format-to-sxml first) ,(apply format-to-sxml rest))]))
