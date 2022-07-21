;; Commentary

;; This file gets loaded directly by the C runtime.
;; Its job is to load the user's config file.

;; Code

(define core-modules-path "guile/")
(add-to-load-path core-modules-path)

(define-public (user-config-home)
	"Return the path of the user's config directory.
This will either be ~/.config/ or $XDG_USER_HOME, if it is set. "
	(or (getenv "XDG_CONFIG_HOME")
		(string-append (passwd:dir (getpw (getuid)))
					   "/.config")))

(define-public (load-user-config-file file)
	"Load a file from the user's configuration directory."
	(load (string-append (user-config-home) "/mintywm/" file)))
