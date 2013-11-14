;;; ORG mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; (require 'remind-to-diary)
;; (require 'org-exp-bibtex)

;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (org . t)
   (latex . t)
   (haskell . t)
   (clojure . t)
   (ditaa . t)))

;;; Org-mode table for plain text including Latex
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;;; org with latex.
(require 'org-latex)
(setq org-export-latex-listings t)
;; (add-to-list 'org-export-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-export-latex-packages-alist '("" "color"))
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e,longtable,float,wrapfig,soul,textcomp,marvosym,latexsym,color,listings}
\\usepackage{amsmath,amssymb,graphicx,setspace,fullpage,authblk,epstopdf,tabularx}
\\usepackage{natbib}
\\usepackage[backref=page,citecolor=blue,bookmarks=false,colorlinks=true,pdfstartview=FitH]{hyperref}
\\newcommand*{\\doi}[1]{\\href{http://dx.doi.org/#1}{doi: #1}}
\\renewcommand*{\\sectionautorefname}{Section}
\\doublespacing
\\tolerance=1000
\\allowdisplaybreaks
\\newtheorem{theorem}{Theorem}
\\newtheorem{lemma}[theorem]{Lemma}
\\newtheorem{cor}[theorem]{Corollary}
\\newtheorem{prop}{Proposition}
               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-preview-ltxpng-directory "./ltxpng/")


;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on


(global-set-key (kbd "C-c i") 'org-time-stamp-inactive)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done 'time)
;; (setq org-log-done 'note)
;; (setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Swith entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging.
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;; org with bibtex
(setq org-export-latex-hyperref-format "\\autoref{%s}")

;; (defun org-mode-reftex-search ()
;;   ;;jump to the notes for the paper pointed to at from reftex search
;;   (interactive)
;;   (org-open-link-from-string (format "[[%s]]" (reftex-citation t))))

;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name) (file-exists-p (buffer-file-name))
;;        (progn
;; 	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
;; 	 (global-auto-revert-mode t)
;; 	 (reftex-parse-all)
;; 	 ;add a custom reftex cite format to insert links
;; 	 (reftex-set-cite-format
;; 	  '((?b . "[[bib:%l][%l-bib]]")
;; 	    (?n . "[[notes:%l][%l-notes]]")
;; 	    (?p . "[[papers:%l][%l-paper]]")
;; 	    (?t . "%t")
;; 	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
;;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

;; (defun string-replace (this withthat in)
;;   "replace THIS with WITHTHAT' in the string IN"
;;   (with-temp-buffer
;;     (insert in)
;;     (goto-char (point-min))
;;     (replace-string this withthat)
;;     (buffer-substring (point-min) (point-max))))

;; (defun org-papers-complete-link (&optional arg)
;;   "Create a papers link using completion."
;;   (let (file link)
;;        (setq file (read-file-name "papers: " "papers/"))
;;        (let ((pwd (file-name-as-directory (expand-file-name ".")))
;;          (pwd1 (file-name-as-directory (abbreviate-file-name
;;                         (expand-file-name ".")))))
;;          (setq file (string-replace "papers/" "" file))
;;          (setq file (string-replace pwd "" (string-replace pwd1 "" file)))
;;          (setq file (string-replace ".pdf" "" file))
;;          (setq link (org-make-link "papers:" file)))
;;     link))

;; (defun org-mode-reftex-setup ()
;;   (interactive)
;;   (and (buffer-file-name) (file-exists-p (buffer-file-name))
;;        (progn
;;         ; Reftex should use the org file as master file. See C-h v TeX-master for infos.
;;         (setq TeX-master t)
;;         (turn-on-reftex)
;;         ; don't ask for the tex master on every start.
;;         (reftex-parse-all)
;;         ;add a custom reftex cite format to insert links
;;         (reftex-set-cite-format
;;          '((?b . "[[bib:%l][%l-bib]]")
;;            (?n . "[[notes:%l][%l-notes]]")
;;            (?p . "[[papers:%l][%l-paper]]")
;;            (?t . "%t")
;;            (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
;;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; (defun org-bib-complete-link (&optional arg)
;;   "Create a bibtex link using reftex autocompletion."
;;   (org-make-link "bib:" (reftex-do-citation nil t "bib")))

;; (defun org-mode-reftex-setup ()
;;   (setq TeX-master t)
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (progn
;;      (reftex-parse-all)
;;      ;; (reftex-set-cite-format "[[cite:%l][%l]]")
;; ))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
;;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (progn
	 (reftex-parse-all)
	 (reftex-set-cite-format 'natbib)
))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'reftex-reference)
)
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq reftex-default-bibliography
      (quote
       ("default.bib" "D:/Dropbox/latex/MyLibrary.bib")))

;;; org-cdlatex-mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;;; external program to open files in ORG
(add-hook 'org-mode-hook
      '(lambda ()
         (setq org-file-apps
           '((auto-mode . emacs)
             ("\\.mm\\'" . default)
             ("\\.doc\\'" . default)
             ("\\.docx\\'" . default)
             ("\\.x?html?\\'" . default)
             ("\\.pdf\\'" . "SumatraPDF %s")))))

;;; Org Capture
 (require 'org-capture)
(setq org-default-notes-file (concat org-directory "D:/Dropbox/org/notes.org"))
(setq org-default-notes-file "D:/Dropbox/org/notes.org")
(setq org-export-html-validation-link "")


;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)

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

;; ;; No Christan, Hebrew, Islamic Holiday
;; (setq christian-holidays nil
;;       hebrew-holidays nil
;;       islamic-holidays nil
;;       solar-holidays nil
;;       bahai-holidays nil
;;       )
(setq calendar-holiday '((holiday-fixed 1 1 "New Year")))

(custom-set-variables
 '(org-agenda-files (quote ("D:/Dropbox/Org/jobs.org"
                            "D:/Dropbox/Org/Personal.org"
			      "D:/Dropbox/Org/Diary/Birthday.org"
                            "D:/Dropbox/Org/Research.org")))

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


;; ;;; ;;Add the Chinese Holidays and Canadian Holidays
;; (icalendar-import-file "C:/My Program Files/GNU Emacs 24.2/site-lisp/site-start.d/Canada-Holidays.ics"
;;                        "C:/My Program Files/GNU Emacs 24.2/site-lisp/site-start.d/Canada-Holidays")
;; (icalendar-import-file "C:/My Program Files/GNU Emacs 24.2/site-lisp/site-start.d/China-Holidays.ics"
;;                        "C:/My Program Files/GNU Emacs 24.2/site-lisp/site-start.d/China-Holidays")
