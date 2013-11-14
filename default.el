;;; Set the home directory
;;; (setenv "HOME" "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d")
;;; (setenv "PATH" "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d")
;; ;;set the default file path
;; (setq default-directory "~/")

;;; Nice options to have On by default
(global-font-lock-mode t)	; syntax highlighting
(global-linum-mode 1)
;; (transient-mark-mode t)	; sane select (mark) mode
(delete-selection-mode t)	; entry deletes marked text
(show-paren-mode t)		; match parentheses
(add-hook 'text-mode-hook 'turn-off-auto-fill) ; wrap long lines in text mode
(tool-bar-mode -1)		; hide the toolbar
(mouse-avoidance-mode 'jump)	; mouse jumps when cursor is too close
(global-auto-revert-mode 1)
(defalias 'make-local-hook 'ignore)
(global-set-key [f12] 'auto-fill-mode)
(setq ess-sas-global-unix-keys t)
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq system-time-locale "C")

;; Emacs package management
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;;; for thesaurus
(require 'thesaurus)
(setq thesaurus-bhl-api-key "aa8038cf0253f7eb703c14d0d657e805")
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

;;;
;;; Easier printing
;;;
(require 'w32-winprint)
(require 'htmlize-view)
(htmlize-view-add-to-files-menu)

;;;; Showing agenda view at start-up
(add-hook 'after-init-hook '(lambda () (org-agenda-list 1)))
;; show file size (emacs 22+)
(when (fboundp size-indication-mode)
  (size-indication-mode t))

;;; show the full path of the current file
(setq frame-title-format
      '("%S" (buffer-file-name "%f"
			       (dired-directory dired-directory "%b"))))

;; Setup bookmarks file
(setq bookmark-default-file "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/bookmarks" bookmark-save-flag 1)
(add-to-list 'load-path "d:/Program Files/GNU Emacs 24.3/site-lisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "d:/Program Files/GNU Emacs 24.3/site-lisp/auto-complete//ac-dict")
(ac-config-default)



(global-auto-complete-mode t)
;; ac-math
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands ac-source-yasnippet)
               ac-sources)))

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)

(ac-flyspell-workaround)

(setq ac-math-unicode-in-math-p t)
(defun ac-latex-mode-setup ()
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-latex-commands)
               ac-sources)))


;;;
;;; ESS
;;;
;; Load ESS and activate the nifty feature showing function arguments
;; in the minibuffer until the call is closed with ')'.
(require 'ess-site)
(require 'ess-eldoc)

(setq-default inferior-R-args "--no-save ")
(setq-default inferior-R-program-name
              "C:/Program Files/R/R-3.0.1/bin/x64/rterm.exe")

(setq-default ess-dialect "R")
(defun ess-set-language ()
  (setq-default ess-language "R")
  (setq ess-language "R")
  )
  
  ;;; Define the sas start location
(setq-default ess-sas-submit-command "D:/SAS92/SASFoundation/9.2/sas.exe -config 'D:/SAS92/SASFoundation/9.2/NLS/EN/SASV9.CFG'")



;;;
;;; AUCTeX
;;;
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)
(require 'font-latex)


;;;
;;; SVN
;;;
(add-to-list 'vc-handled-backends 'SVN)
(require 'psvn)


;;;
;;; Use Aspell for spell checking
;;;
(setq-default ispell-program-name "D:/Program Files/GNU Emacs 24.3/aspell/bin/aspell.exe")
;; (setq ispell-dictionary "british")

;;;
;; Emacs will load all ".el" files in "

(mapc 'load
     (directory-files "D:/Program Files/GNU Emacs 24.3/site-lisp/site-start.d" t "\\.el\\'"))
;;; mouse scrolling setting
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(setq column-number-mode t)                   ;; show column numbers
;; (size-indication-mode t)                 ;; show file size (emacs 22+)
(require 'ido)

(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/.ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t  ; wait for RET, even with unique completion
)
(setq visible-bell t)
(require 'server)


;; use Y or N for Yes and No
(fset 'yes-or-no-p 'y-or-n-p)
;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;; (require 'color-theme)
(load-theme 'misterioso)
;;; another color theme
;; (require 'tmtheme)
;; (setq tmtheme-directory "D:/Program Files/GNU Emacs 24.2/site-lisp/site-start.d/tmthemes")
;; (tmtheme-scan)
;; spell check
(add-hook 'text-mode-hook 'flyspell-mode)
;;;; Maximize the frame at startup
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
;; (set-frame-size-according-to-resolution)
;;; CDlatex
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)  ; with AUCTeX Latex

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning))
(setq session-save-file "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/.session")
;; (load "desktop")
(require 'desktop)
(desktop-save-mode 1)
(setq history-length 150)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(setq desktop-buffers-not-to-save
      (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	        "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; ;;;; use only one desktop
(setq desktop-path '("D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/"))
(setq desktop-dirname "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/")
(setq desktop-base-file-name ".emacs-desktop")

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(if (file-exists-p ".emacs.desktop")
   (progn (setq desktop-path '("."))
	   (desktop-save-mode 1)
	   )
 )

(setq auto-save-list-file-prefix "D:/Program Files/GNU Emacs 24.3/site-lisp/.emacs.d/auto-save-list/.saves-")

;;; Set cursor color to white
(set-cursor-color "#e6e6fa")
(defalias 'make-local-hook 'ignore)
(global-hl-line-mode 1)

;; fontsize
;; (set-face-attribute 'default nil :height 170)
;; (set-face-attribute 'default nil :font "Monaco-14")
(set-default-font "Inconsolata-16")

;;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 18)))
