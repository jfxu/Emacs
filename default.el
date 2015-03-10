;;; default.el --- Default configuration for GNU Emacs
;;; Used mainly to load custom extensions.
;;; (Loaded *after* any user and site configuration files)

;; Copyright (C) 2014 Vincent Goulet

;; Author: Vincent Goulet

;; This file is part of Emacs for Windows Modified
;; http://vgoulet.act.ulaval.ca/emacs

;; GNU Emacs for Windows Modified is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;
;;; Version number of Emacs Modified
;;;
;; Define variable and function 'emacs-modified-version'
(require 'version-modified)

;;;
;;; Easier printing
;;;
(require 'w32-winprint)
(require 'htmlize-view)
(htmlize-view-add-to-files-menu)

;;;
;;; ESS
;;;
;; Load ESS and activate the nifty feature showing function arguments
;; in the minibuffer until the call is closed with ')'.
(require 'ess-site)

;;;
;;; AUCTeX
;;;
;; Load AUCTeX and preview-latex.
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)

(custom-set-variables '(markdown-command "Pandoc"))
;;;;;;;;;;;;   
;;; polymode
;;;
;; Activation of the R specific bundle and basic configuration.
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.rapport" . poly-rapport-mode))
(add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rbrew" . poly-brew+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
(require 'poly-R)

;;;
;;; SVN
;;;
;; Support for the Subversion version control system
;; (http://http://subversion.tigris.org/) in the VC backend. Use 'M-x
;; svn-status RET' on a directory under version control to update,
;; commit changes, revert files, etc., all within Emacs. Requires an
;; installation of Subversion in the path.
(add-to-list 'vc-handled-backends 'SVN)
(require 'psvn)

;;;
;;; Use Aspell for spell checking
;;;
(setq-default ispell-program-name "C:/Program Files (x86)/GNU Emacs 24.4/aspell/bin/aspell.exe")

;;;
;;; Other extensions
;;;
;; Emacs will load all ".el" files in 
;;   C:/Program Files (x86)/GNU Emacs 24.4/share/emacs/site-lisp/site-start.d/
;; on startup.
(mapc 'load
      (directory-files "C:/Program Files (x86)/GNU Emacs 24.4/share/emacs/site-lisp/site-start.d" t "\\.el\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Personal Settings ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode t)	; syntax highlighting
(global-linum-mode 1)           ; Show line numbers
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

;;;;; Emacs package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;; ESS settings
(setq-default ess-dialect "R")
(defun ess-set-language ()
  (setq-default ess-language "R")
  (setq ess-language "R")
  )
;;; Make ESS work right with ggplot2
(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-first-continued-statement-offset 4)
            (setq ess-continued-statement-offset 0)))
  
  ;;; Define the sas start location
(setq-default ess-sas-submit-command "D:/SAS92/SASFoundation/9.2/sas.exe -config 'D:/SAS92/SASFoundation/9.2/NLS/EN/SASV9.CFG'")

;;;;;;;; For auto-complete  ;;;;;;;
;;(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150201.150")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150201.150/dict")
(require 'auto-complete-config)
(ac-config-default)
(setq ess-use-auto-complete 'script-only)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)
;; (global-auto-complete-mode t)


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
(setq bookmark-default-file "~/.emacs.d/bookmarks" bookmark-save-flag 1)


(setq visible-bell t)
;;; (require 'server)

;; use Y or N for Yes and No
(fset 'yes-or-no-p 'y-or-n-p)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'color-theme)
(load-theme 'misterioso)

;;;; Maximize the frame at startup
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning))
(setq session-save-file "~/.emacs.d/.session")

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


;;; Set cursor color to white
(set-cursor-color "#e6e6fa")
(defalias 'make-local-hook 'ignore)
(global-hl-line-mode 1)

;; fontsize
;; (set-face-attribute 'default nil :height 170)
;; (set-face-attribute 'default nil :font "Monaco-14")
(set-default-font "Inconsolata-16")
;; (set-frame-font "Inconsolata-14")
;; (set-frame-font "DejaVu Sans Mono-12")
;; (set-default-font "DejaVu Sans Book-14")

;;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 18)))
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ORG settings

(global-set-key (kbd "C-c i") 'org-time-stamp-inactive)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)

;; ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "D:/Dropbox/org/notes.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "Reply" entry (file "D:/Dropbox/org/notes.org")
               "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "Note of research" entry (file "D:/Dropbox/org/newidea.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "D:/Dropbox/org/notes.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "D:/Dropbox/org/notes.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("p" "Appointment" entry (file "D:/Dropbox/org/appointments.org" "Calendar")
	       "* APPT %^{Description} %^g %? Added: %U")
               ;; "* TODO %? :Appointment:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "D:/Dropbox/org/notes.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(custom-set-variables
;;'(org-agenda-files (quote ("D:/Dropbox/Org/jobs.org"
;;                           "D:/Dropbox/Org/Personal.org"
;;			     "D:/Dropbox/Org/Diary/Birthday.org"
;;                           "D:/Dropbox/Org/Research.org")))
 '(org-agenda-files (quote ("D:/Dropbox/Org/Personal.org"
			    "D:/Dropbox/Org/Diary/Birthday.org")))
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 ;; '(org-startup-folded nil)
 ;; '(org-log-done t)
 '(org-todo-keywords '((sequence "TODO(t)"  "STARTED(s)" "APPOINTMENT(a)" "WAITING(w)"
                                 "|" "DONE(d)" "CANCELED(c)")))
 '(org-agenda-use-time-grid t)
 '(org-hide-leading-stars t)
 '(org-agenda-include-diary t)
 '(org-agenda-show-current-time-in-grid t)
 '(org-upcoming-deadline '(:foreground "blue" :weight bold))
 '(org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "dark magenta" :weight bold)
	      ("APPOINTMENT" :foreground "purple" :weight bold)
              ("WAITING" :foreground "DeepPink3" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELED" :foreground "forest green" :weight bold)
	      )))
 '(org-use-fast-todo-selection t)
 ;; '(org-agenda-skip-scheduled-if-done t)
 ;; '(org-agenda-skip-deadline-if-done t)
 ;; '(org-agenda-include-all-todo t)
 ;; '(org-use-property-inheritance t)
 ;; '(org-enforce-todo-dependencies t)
 ;; '(org-special-ctrl-a/e t)
 ;; '(org-special-ctrl-k t)
 ;; '(org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
 ;; '(org-agenda-dim-blocked-tasks 'invisible)
 ;; '(org-enforce-todo-checkbox-dependencies t)
 ;; '(org-completion-use-iswitchb t)
 ;; '(org-export-allow-BIND t)
 '(diary-file "D:/Dropbox/Org/Diary/China-Holidays"
              "D:/Dropbox/Org/Diary/Canada-Holidays")
 '(mark-diary-entries-in-calendar t)
 '(org-export-html-postamble t)
 '(org-export-html-postamble-format (quote (("en" "<hr><p><span class=\"author\">Last Updated by %a </span> <span class=\"date\">on %d </span></p>"))))
 ;; '(org-directory "D:/Dropbox/Org")
;;  '(org-agenda-files (directory-files org-directory t ".*\.org$"))
 )
 
;;; ;;Add the Chinese Holidays and Canadian Holidays
;; (icalendar-import-file "D:/Dropbox/Org/Diary/CHN.ics"
;; 			"D:/Dropbox/Org/Diary/China-Holidays")
;; (icalendar-import-file "D:/Dropbox/Org/Diary/CAN-Ontario.ics"
;;			"D:/Dropbox/Org/Diary/Canada-Holidays")