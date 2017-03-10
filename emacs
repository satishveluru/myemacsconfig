;========================================================================================
;  This file must be named ".emacs" and place it in  your $HOME directory.
;========================================================================================


;============================================
;  No GNU Emacs / welcome buffer
;============================================
(setq inhibit-startup-screen t)

(package-initialize)
(package-refresh-contents)
(require 'package)

;============================================
;  Set up Network Proxies, if needed...
;     localhost:3128 is for local squid
;============================================
(setq url-proxy-services
      '(
		("www"      . "localhost:3128")
		("http"     . "localhost:3128")
		("https"    . "localhost:3128")
		))

;=======================================================
; Package management
;=======================================================
(require 'package)
(setq package-archives nil)
(add-to-list 'package-archives '("gnu"           . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade"     . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"         . "https://melpa.org/packages/"))

(package-refresh-contents)


