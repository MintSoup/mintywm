(define user-config-home
	(or (getenv "XDG_CONFIG_HOME")
		(string-append (passwd:dir (getpw (getuid)))
					   "/.config")))

(define (load-user-config-file file)
	(load (string-append user-config-home "/mintywm/" file)))

(load-user-config-file "init.scm")
