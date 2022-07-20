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

;; This file contains a bunch of utility functions for mintywm.

;; Code

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
	"Sleep for n seconds in the current thread."
	(srfi18:thread-sleep!
	 (srfi18:seconds->time
	  (+ n (srfi18:time->seconds (srfi18:current-time))))))

(define* (set-timeout trunk n #:optional (repeat #f))
	"Run trunk after n seconds.
If repeat is #t, run trunk every n seconds without end.

The trunk will be called in a new thread returned by this function."
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
	"Get the delay of task."
	(car task))

(define (task-trunk task)
	"Get the trunk of task."
	(cdr task))

(define (make-task delay trunk)
	"Make a new task object with the given delay and trunk."
	(cons delay trunk))

(define (current-time-seconds)
	"Return the current time in seconds."
	(srfi18:time->seconds (srfi18:current-time)))

(define (make-scheduler task-objs)
	"Run a task scheduler which runs the given tasks.
task-objs is a list of task objects, constructed by make-task.

task-objs is a list where each element is a task, created using make-task.

At first, all tasks will be run. After that, each task will run periodically
with its specified time delay.

All tasks run in the current thread.

Example:
(make-scheduler
  (list
    (make-task 5 (lambda () (display \"hello\")))
    (make-task 12 (lambda () (display \"world\")))))

will print \"hello\" every 5 seconds and \"world\" every 12 seconds.

This function is used internally by make-bar-updater."
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
	"Run cmd synchronously in a shell and return its output.
If max-len is given, return only the first max-len characters of the output.

Example:
(get-proc-output \"date --utc\")
  => Wed Jul 20 11:52:06 AM UTC 2022
"
	(let* ((port (open-input-pipe cmd))
		   (str (get-string-n port max-len)))
		(catch #t
			(lambda ()
				(close-pipe port))
			(lambda* (#:rest o)
				*unspecified*))
		str))
