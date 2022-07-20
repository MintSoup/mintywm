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
(define-module (mintywm utils)
	#:use-module ((srfi srfi-18) #:prefix srfi18:)
	#:use-module (srfi srfi-1)
	#:use-module (srfi srfi-17)
	#:use-module (ice-9 popen)
	#:use-module (ice-9 textual-ports)

	#:export
	(format-time
	 sleep-for
	 set-timeout
	 task-delay
	 task-trunk
	 make-task
	 current-time-seconds
	 make-scheduler
	 get-proc-output
	 getf))

(define (local-time)
	(localtime (truncate (srfi18:time->seconds (srfi18:current-time)))))

(define (format-time str)
	"Run strftime on the given string using current time.
For more info, see strftime(3)."
	(strftime str (local-time)))

(define (sleep-for n)
	(srfi18:thread-sleep!
	 (srfi18:seconds->time
	  (+ n (srfi18:time->seconds (srfi18:current-time))))))

(define* (set-timeout trunk n #:optional (repeat #f))
	(let ((thread
		   (srfi18:make-thread
			(lambda ()
				(let loop ()
					(sleep-for n)
					(trunk)
					(if repeat (loop)))))))
		(srfi18:thread-start! thread)
		thread))

(define (task-delay task)
	(car task))

(define (task-trunk task)
	(cdr task))

(define (make-task delay trunk)
	(cons delay trunk))

(define (current-time-seconds)
	(srfi18:time->seconds (srfi18:current-time)))

(define (make-scheduler task-objs)
	(define task-next-time
		(getter-with-setter car set-car!))

	(define (task-obj task)
		(cdr task))

	(define tasks
		(map
		 (lambda (task)
			 (cons 0 task))
		 task-objs))

	(define tasks-to-do tasks)

	(let loop ((ct (current-time-seconds)))
		(for-each
		 (lambda (task)
			 ((task-trunk (task-obj task)))
			 (set! (task-next-time task) (+ ct (task-delay (task-obj task)))))
		 tasks-to-do)

		(let ((waiting-time
			   (fold
				(lambda (task prev)
					(min (- (task-next-time task) ct)
						 prev))
				100000
				tasks)))

			(set! tasks-to-do
				  (filter
				   (lambda (task)
					   (<= (task-next-time task)
						   (+ ct waiting-time)))
				   tasks))
			(sleep-for waiting-time)
			(loop (current-time-seconds)))))

(define* (get-proc-output cmd #:optional (max-len 512))
	(let* ((port (open-input-pipe cmd))
		   (str (get-string-n port max-len)))
		(catch #t
			(lambda ()
				(close-pipe port))
			(lambda* (#:rest o)
				*unspecified*))
		str))
