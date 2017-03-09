;========================================================================================
;  This file must be named ".emacs" and live in your $HOME directory.
;========================================================================================

;--------------------------------------------------------------------- 
;  Locate our e-lisp...
;---------------------------------------------------------------------
(setenv "SITELISP_HOME" "~/site-lisp")
(add-to-list 'load-path (getenv "SITELISP_HOME"))

;============================================
;  Load my functions...
;============================================
(load-library "~/.emacs-fncs.el")

;============================================
;  No GNU Emacs / welcome buffer
;============================================
(setq inhibit-startup-screen t)

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

;;; (setq debug-on-error t)
(package-initialize)
(package-refresh-contents)

