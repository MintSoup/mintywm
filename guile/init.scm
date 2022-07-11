(use-modules (system repl server))
(use-modules (srfi srfi-18))

(define (local-time)
	(localtime (truncate (time->seconds (current-time)))))

(define (get-status)
	(string-append
	 (get-time)
	 " | "
	 (get-date)))

(define (get-date)
	(strftime "%a, %d %b %Y" (local-time)))

(define (get-time)
	(strftime "%r" (local-time)))

(define (sleep-for n)
	(thread-sleep!
	 (seconds->time
	  (+ n (time->seconds (current-time))))))

(define (bar-updater)
	(set-title (get-status))
	(sleep-for 1)
	(bar-updater))

(bar-updater)

;; (define bar-thread (make-thread bar-updater))
;; (thread-start! bar-thread)
