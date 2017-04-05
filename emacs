;---------------------------------------------------------------------
;  Locate our e-lisp...
;---------------------------------------------------------------------

(setenv "SITELISP_HOME" "~/site-lisp")
(add-to-list 'load-path (getenv "SITELISP_HOME"))


;-------------------------------------------------------
;  Show date and time in mode line
;-------------------------------------------------------

(setq display-time-day-and-date t)
(display-time)


;============================================
;  No welcome buffer Remove Menu toool bar and scroll bar
;============================================
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

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

;(setq package-archives nil)
(setq package-enable-at-startupa nil)
(add-to-list 'package-archives '("gnu"           . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-milkbox" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade"     . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"         . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" 	 . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes nil)
 '(package-selected-packages
   (quote
    (which-key try use-package flycheck find-file-in-repository autopair magit python-mode font-lock-profiler font-utils font-lock-studio font-lock+ jedi elpy auto-complete solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(require 'solarized-dark-theme)

;=======================================================
; Python Setting
;=======================================================
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;==================================
;Enable elpy Packages
;==================================
(elpy-enable)


(put 'upcase-region 'disabled nil)

(require 'setup-cygwin)

;============================================
;  Shell mode
;============================================
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (defun make-my-shell-output-read-only (text)
;;   "Add to comint-output-filter-functions to make stdout read only in my shells."
;;   (if (member (buffer-name) my-shells)
;;       (let ((inhibit-read-only t)
;;             (output-end (process-mark (get-buffer-process (current-buffer)))))
;;         (put-text-property comint-last-output-start output-end 'read-only t))))
;; (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-shell-setup ()
  (setq comint-scroll-show-maximum-output 'this)
  (setq comint-completion-addsuffix       t)
  (setq comint-eol-on-send                t)
  (setq w32-quote-process-args            ?\")

  (make-variable-buffer-local   'comint-completion-addsuffix)
  (local-set-key '[up]          'comint-previous-input)
  (local-set-key '[down]        'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
)
(setq shell-mode-hook 'my-shell-setup)
(setq process-coding-system-alist (cons '("bash" . raw-text-unix)
                                        process-coding-system-alist))

;;===========================
;; Configurations changes
;;===========================

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).

(let* ((cygwin-root "C:/cygwin64")
         (cygwin-bin (concat cygwin-root "/bin")))
    (when (and (eq 'windows-nt system-type)
  	     (file-readable-p cygwin-root))

    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

;; By default use the Windows HOME.
;; Otherwise, uncomment below to set a HOME
;; (setenv "HOME" (concat cygwin-root "/home/eric"))


;; NT-emacs assumes a Windows shell. Change to bash.
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
;;;(setq explicit-shell-file-name shell-file-name)
(setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")


 ;; This removes unsightly ^M characters that would otherwise
 ;; appear in the output of java applications.
 (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(set-cursor-color     "orange")
(set-mouse-color      "#bf3eff")
(setq initial-scratch-message "")

;; Remove whitespaces before saving file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; To move one screen to Other Using Shift arrow key command.
(windmove-default-keybindings 'shift)

;; To search files or directories it will be useful
(ido-mode t)
(setq ido-show-dot-for-dired t)
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)

;; To show Buffer list in meaningful way
(defalias 'list-buffers 'ibuffer-other-window)

;---------------------------------------------------------------------
;  Re-map some keys
;---------------------------------------------------------------------
(global-set-key "\C-C%"     'replace-regexp-region)
(global-set-key "\C-Ca"     'align-words-align)
(global-set-key "\C-Cc"     'comment-region)
(global-set-key "\C-Ce"     'eval-buffer)
(global-set-key "\C-Cf"     'fixup-whitespace)
(global-set-key "\C-Cg"     'grep-find)
(global-set-key "\C-Ci"     'indent-region)
(global-set-key "\C-Ck"     'kill-region)
(global-set-key "\C-Cl"     'linum-mode)
(global-set-key "\C-Co"     'find-file-at-point)
(global-set-key "\C-Cq"     'c-fill-paragraph)
(global-set-key "\C-Cr"     'revert-buffer)
(global-set-key "\C-Cs"     'sort-lines)
(global-set-key "\C-Cu"     'uncomment-region)
(global-set-key "\C-C="     'align-equal-signs-region)
(global-set-key "\C-Z"      'goto-line)
(global-set-key "\C-c\C-l"  'downcase-letter)
(global-set-key "\C-c\C-u"  'upcase-letter)



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


