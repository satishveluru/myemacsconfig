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

;============================================
;  Font, colors, sizes, ...
;============================================
;;; (set-default-font "-misc-fixed-bold-r-normal--20-140-100-100-c-100-iso8859-1")
;;; (set-default-font "-*-Terminal-normal-r-*-*-12-90-*-*-c-80-*-oem-")
;;; (set-default-font "-*-Fixedsys-normal-r-*-*-15-112-*-*-c-80-*-ansi-")
;;; (set-default-font "-*-Lucida Sans Typewriter-bold-r-*-*-14-*-*-*-c-*-*-ansi-")
;;; (set-default-font "-*-Lucida Sans Typewriter-bold-r-*-*-11-*-*-*-c-*-*-ansi-")
;;; (set-default-font "-b&h-lucidatypewriter-medium-r-normal-sans-*-120-*-*-*-*-iso8859-1")
;;; (set-default-font "lucidasanstypewriter-10")
;;; (set-default-font "-misc-droid sans mono-medium-r-normal--0-0-0-0-m-0-iso8859-1")
;;; (set-default-font "-b&h-lucidabright-medium-r-normal--10-100-75-75-p-56-iso8859-1")
;;; (set-default-font "-b&h-lucidatypewriter-medium-r-normal-sans-12-100-75-75-m-60-iso8859-1")
;;; (set-default-font "-raster-Fixedsys-normal-r-normal-normal-12-90-96-96-c-80-iso10646-1")
;;; (set-default-font "-*-Terminal-normal-r-*-*-12-90-*-*-c-80-*-oem-")
;;; (set-default-font "-*-Lucida Sans Typewriter-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
;;; (set-default-font "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
;;; (set-default-font "-outline-Droid Sans Mono-normal-r-normal-normal-14-*-96-96-c-*-iso8859-1")
;;; (set-default-font "-*-Lucida Console-normal-r-*-*-12-*-*-*-*-*-*-*")
;;; (set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-8-80-75-75-m-50-iso10646-1")
(set-default-font "-misc-droid sans mono-medium-r-normal--13-0-0-0-m-0-iso8859-1")

(set-cursor-color     "orange")
(set-mouse-color      "#bf3eff")
;;; (set-foreground-color "Snow2")
;;; (set-background-color "Black")

;--------------------------------------------
; Locate and size main frame...
;--------------------------------------------
;;; (monitor-resize-current-frame 200 100 100 100)

(setq default-tab-width 4)

(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;-------------------------------------------------------
;  Provides end-of-line conversions (DOS, UNIX, Mac),
;  whitespace handling.
;-------------------------------------------------------
(require 'eol-conversion)
(require 'whitespace)

;-------------------------------------------------------
;  Show date and time in mode line
;-------------------------------------------------------
(setq display-time-day-and-date t)
(display-time)

;=======================================================
;  Used to align columns of text
;  or anything based on a regexp
;=======================================================
(require 'align-words)
(require 'align)

;---------------------------------------------------------------------
;  Special saving on Windoze (mostly) .. remove tabs.
;---------------------------------------------------------------------
(setq indent-tabs-mode nil)

;;; (defun tomh-windows-save()
;;;   (untabify (point-min) (point-max)) 
;;; )
;;; (add-hook 'write-file-hooks 'tomh-windows-save)

;;; (setq default-process-coding-system  '(undecided-dos . undecided-unix))

;---------------------------------------------------------------------
;  Windoze/Cygwin setup
;---------------------------------------------------------------------
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
;; (let* ((cygwin-root "c:/cygwin64")
;; 	   (cygwin-bin (concat cygwin-root "/bin")))
;;   (when (and (eq 'windows-nt system-type)
;; 			 (file-readable-p cygwin-root))
;;     
;; 	(setq exec-path (cons cygwin-bin exec-path))
;; 	(setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
;;     
;; 	;; NT-emacs assumes a Windows shell. Change to bash.
;; 	(setq shell-file-name "bash")
;; 	(setenv "SHELL" shell-file-name) 
;; 	(setq explicit-shell-file-name shell-file-name) 
;;     
;; 	;; This removes unsightly ^M characters that would otherwise
;; 	;; appear in the output of java applications.
;; 	(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
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

;;; (setq shell-prompt-pattern     "^[^#$%>\n]*[#$%>] *")
;;; (setq shell-prompt-pattern "\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ ")
;;; export PS1="\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ "

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m t)

(shell)

;--------------------------------------------
;  Color themes...
;--------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/solarized-theme-20161222.109a/")
(require 'color-theme)
(require 'color-theme-autoloads)
(require 'solarized-dark-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;;; (color-theme-charcoal-black) ;;; or -midnight

;--------------------------------------------
;--  Turn on font-lock colorization
;--------------------------------------------
(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (setq font-lock-face-attributes
         ;; Symbol-for-Face Foreground Background Bold Italic Underline
         '(
           (font-lock-comment-face       "DarkSlateGrey")
           (font-lock-string-face        "DarkGoldenrod3")
           (font-lock-keyword-face       "IndianRed3")
           (font-lock-function-name-face "Magenta")
           (font-lock-variable-name-face "SlateBlue")
           (font-lock-type-face          "DarkGreen")
           (font-lock-constant-face      "Purple")
           ))

;;;           (font-lock-api-face       "LightSlateBlue")
;;;           (font-lock-bold-face      "Aquamarine")
;;;           (font-lock-doc-tag-face   "DarkSeaGreen")
;;;           (font-lock-link-face      "DodgerBlue")
;;;           (font-lock-constant-face  "CornflowerBlue")
;;;           (font-lock-modifier-face  "IndianRed")
;;;           (font-lock-operator-face  "DarkOrchid2")
;;;           (font-lock-private-face   "springgreen4")
;;;           (font-lock-protected-face "springgreen3")
;;;           (font-lock-public-face    "red")


       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ))

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;--------------------------------------------
;--  Incremental highlighting of searches
;--------------------------------------------
(load-library "ishl.elc")
(ishl-mode)
(setq query-replace-highlight t)
(setq search-highlight        t)

;--------------------------------------------
;--  Cursor jiggling
;--------------------------------------------
(load-library "jiggle.elc")
(jiggle-mode 1)
(jiggle-searches-too 1)
(setq jiggle-how-many-times 8)


;---------------------------------------------------------------------
;  JavaScript mode setup...
;---------------------------------------------------------------------
(require 'js3-mode)
(setq auto-mode-alist (append '(("\\.js$" . js3-mode)) auto-mode-alist))

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

;---------------------------------------------------------------------
;  Set up JDE -- Java Development Environment
;---------------------------------------------------------------------
(add-hook 'java-mode-hook 
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'java-mode-untabify)))

(c-add-style "Java-Tom" 
             '("Java"
               (c-basic-offset . 4)
               (c-offsets-alist
                (arglist-intro  . +)
                (arglist-close  . 0)
                (substatement-open . 0))))

(require 'java-mode-indent-annotations)

(add-hook 'java-mode-hook   'tomh-set-format)

(add-hook 'java-mode-hook   'eclim-mode)
(setq eclim-executable "c:/Tools/eclipse/kepler/eclim.cmd")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
;;; (autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
;;; (add-to-list 'auto-mode-alist '("\.groovy$"         . groovy-mode))
;;; (add-to-list 'auto-mode-alist '("\.gant$"           . groovy-mode))
;;; (add-to-list 'auto-mode-alist '("\.gradle"          . groovy-mode))
;;; (add-to-list 'auto-mode-alist '("gradlefile"        . groovy-mode))
;;; (add-to-list 'auto-mode-alist '("gradlesettings"    . groovy-mode))
;;; (add-to-list 'auto-mode-alist '("gradle.properties" . groovy-mode))

;;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;--------------------------------------------
;  Emacs Server (works with emacsclient)...
;--------------------------------------------
(server-start)

;--------------------------------------------
;  Pseudo JSP mode
;--------------------------------------------
(require 'multi-mode)

(defun jsp-mode () (interactive)
  (multi-mode 1
              'html-mode
              '("<%--"     indented-text-mode)
              '("<%@"      indented-text-mode)
              '("<%="      html-mode)
              '("<%"       java-mode)
              '("%>"       html-mode)
              '("<script"  java-mode)
              '("</script" html-mode)
              ))
              
(setq auto-mode-alist (append '(("\\.jsp$" . jsp-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mar$" . archive-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.aar$" . archive-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.war$" . archive-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ear$" . archive-mode)) auto-mode-alist))

;-------------------------------------------------------
;  Provides CSS editing mode
;-------------------------------------------------------
;;; FIX THIS (autoload 'css-mode "css-mode")
;;; (setq auto-mode-alist       
;;;       (cons '("\\.css\\'" . css-mode) auto-mode-alist))
;;; (require 'sass-mode)

;-------------------------------------------------------
;  Provides PL/SQL editing mode
;-------------------------------------------------------
(autoload 'pls-mode  "pls-mode" "PL/SQL Editing Mode" t)
(setq auto-mode-alist (append '(("\\.sql$"  . pls-mode)) auto-mode-alist))
(setq pls-mode-hook '(lambda () (font-lock-mode 1)))

(require 'plsql)
(setq auto-mode-alist (append '(("\\.sql$"  . plsql-mode)) auto-mode-alist))

;-------------------------------------------------------
;  XML set up
;-------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsd\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

;-------------------------------------------------------
; XSL mode & debugger
;-------------------------------------------------------
(add-to-list 'load-path (concat (getenv "SITELISP_HOME") "/xslide"))
(add-to-list 'load-path (concat (getenv "SITELISP_HOME") "/xslt-process-2.2/lisp"))
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)
(add-hook 'xsl-mode-hook 'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list '("\\.xsl" . xsl-mode))
       auto-mode-alist))

;;; (require 'xslt-process)
;;; (require 'xslt-speedbar)

;;; (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
;;; (autoload 'xslt-process-install-docbook "xslt-process"
;;;    "Register the DocBook package with XSLT-process" t)
;;; (add-hook 'sgml-mode-hook 'xslt-process-mode)
;;; (add-hook 'xml-mode-hook 'xslt-process-mode)
;;; (add-hook 'xsl-mode-hook 'xslt-process-mode)

(defadvice xml-mode (after run-xml-mode-hooks act)
  "Invoke `xml-mode-hook' hooks in the XML mode."
  (run-hooks 'xml-mode-hook))

;--------------------------------------------
; Line Numbering mode...
;--------------------------------------------
(require 'linum)


;--------------------------------------------
; Emacs customizations...
;--------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(archive-zip-extract (quote ("unzip" "-p")))
 '(archive-zip-use-pkzip nil)
 '(auto-image-file-mode t)
 '(browse-url-netscape-program "firefox")
 '(case-fold-search t)
 '(clip-large-size-font t t)
 '(column-number-mode t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(display-time-mode t)
 '(ecb-download-package-version-type 2)
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("~/Projects/checkstyle-src-5.0-beta01")))
 '(ecb-vc-enable-support (quote unless-remote))
 '(ecb-windows-width 0.33)
 '(ediff-diff-options "-wbB")
 '(eshell-debug-command t)
 '(font-list-limit 100)
 '(gnus-select-method
   (quote
	(nntp "news.stardock.com"
		  (nntp news\.eclipse\.com))))
 '(jdee-jdk (quote ("1.7.0_75")))
 '(jdee-jdk-registry
   (quote
	(("1.7.0_75" . "/cygdrive/c/Tools/java/jdk1.7.0_75/"))))
 '(jdee-server-dir "/cygdrive/c/Tools/jdee-server/target/")
 '(js3-indent-level 4)
 '(lpr-command "lpr")
 '(mail-host-address (getenv "COMPUTERNAME"))
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(package-selected-packages
   (quote
	(psvn js3-mode abyss-theme eclim solarized-theme jdee beacon ack-menu ack)))
 '(ps-landscape-mode t)
 '(ps-n-up-printing 2)
 '(ps-printer-name "10.2.4.74")
 '(revert-without-query (quote (".*")))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(url-personal-mail-address "tomh@acm.org")
 '(user-full-name "Tom Halliley")
 '(user-mail-address "jhalliley@creditacceptance.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "DarkSlateGrey"))))
 '(font-lock-doc-face ((t (:foreground "darkslategrey3"))))
 '(minibuffer-noticeable-prompt ((((class color) (min-colors 88) (background light)) (:background "DarkOrchid4" :foreground "white")))))


(pop-to-buffer "*shell*")
