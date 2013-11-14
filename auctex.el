;;; auctex.el

(autoload 'TeX-load-hack
	(expand-file-name "tex-site.el" (file-name-directory load-file-name)))
(TeX-load-hack)

;;; Bib-cite
(autoload 'turn-on-bib-cite "bib-cite")
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
(setq bib-cite-use-reftex-view-crossref t)


(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
;; (setq-default TeX-master nil)
(setq TeX-parse-self t) ;; Enable parse on load
(setq TeX-PDF-mode t)


;;;;;;;;;;;;RefTex;;;;;;;;;;;;;;;;
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-toc-split-windows-horizontally t) ;; horizontal toc buffer
(setq reftex-toc-split-windows-fraction 0.2)   ;;
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)

(setq reftex-cite-format 'natbib)
;(defun reftex-format-autoref (label def-fmt)
;	(format "\\autoref{%s}" label))
;(setq reftex-format-ref-function 'reftex-format-autoref)
(setq reftex-label-alist
	'((nil ?e nil "\\eqref{%s}"   nil nil)  ;; use \eqref   for equation
	  (nil ?s nil "\\autoref{%s}" nil nil)  ;; use \autoref for section
	  (nil ?t nil "\\autoref{%s}" nil nil)  ;; use \autoref for table
	  (nil ?f nil "\\autoref{%s}" nil nil)  ;; use \autoref for figure
))
(setq LaTeX-align-label "eq")
;; (setq reftex-extra-bindings t)

;;;;;;;;;LaTex-mode settings;;;;;
(add-hook 'LaTeX-mode-hook
	(lambda ()
	  ;; (TeX-fold-mode 1)
	  (turn-off-auto-fill)
	  ;; (auto-complete-mode 1)
	  ;; (LaTeX-math-mode 1)	       ;; Latex-math-mode
	  ;; (outline-minor-mode 1)            ;; LaTeX mode outline mode
	  ;; (setq TeX-show-compilation nil)   ;; NOT display compilation windows
	  (setq TeX-global-PDF-mode t)         ;; PDF mode enable, not plain
	  (setq TeX-clean-confirm nil)
	  (setq TeX-save-query -1)
	  (imenu-add-menubar-index)
	  ;; (setq font-latex-fontify-script t)
	  ;; (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
	  ;; (setq TeX-electric-escape t)      ;; mini-buffer
	  ;; (setq TeX-view-program-list '(("SumatraPDF" "SumatraPDF %o")))
	  ;; (setq TeX-view-program-list  '(("Sumatra PDF" ("\"C:/My Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o"))))
	  ;; (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
	  ;; 				     (output-dvi "Yap")))
))

;; (require 'sumatra-forward)

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-master nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 ;; '(LaTeX-command ¡°latex -synctex=1¡å)
 '(TeX-view-program-list (quote (("SumatraPDF"
				  ("\"D:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
				   (mode-io-correlate " -forward-search %b %n") " %o")))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and start")
				      (output-dvi "Yap")
				      (output-pdf "SumatraPDF") (output-html "start")))))
(add-hook 'LaTeX-mode-hook 'orgtbl-mode) ;for orgtbl


;; Auctex fold with section
;;(add-hook 'outline-minor-mode-hook
;;            (lambda () (local-set-key "\C-c\C-1"
;;                                      outline-mode-prefix-map)))))
 ; Outline-minor-mode key map
(setq outline-minor-mode 1)
 (define-prefix-command 'cm-map nil "Outline-")
 ; HIDE
 (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
 (define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
 (define-key cm-map "o" 'hide-other)        ; Hide other branches
 (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
 (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
 (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
 ; SHOW
 (define-key cm-map "a" 'show-all)          ; Show (expand) everything
 (define-key cm-map "e" 'show-entry)        ; Show this heading's body
 (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
 (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
 (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
 ; MOVE
 (define-key cm-map "u" 'outline-up-heading)                ; Up
 (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
 (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
 (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
 (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
 (global-set-key "\M-o" cm-map)

(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))
(setq TeX-outline-extra
      '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
	("\\\\bibliography\\b" 2)))
