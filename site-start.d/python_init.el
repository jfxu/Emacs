;;;; Python-mode
;; (setq py-install-directory "C:\\Users\\Jianfeng\\AppData\\Local\\Enthought\\Canopy\\User\\Scripts")
;; (add-to-list 'load-path py-install-directory)
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setenv "PYTHONPATH" "C:\\Users\\Jianfeng\\AppData\\Local\\Enthought\\Canopy\\User\\python.exe")

(setq
 	python-shell-interpreter "C:\\Users\\Jianfeng\\AppData\\Local\\Enthought\\Canopy\\User\\python.exe"
 	;; python-shell-interpreter "C:\\Python27\\python.exe"
 	python-shell-interpreter-args "-i C:\\Users\\Jianfeng\\AppData\\Local\\Enthought\\Canopy\\User\\Scripts\\ipython-script.py"
 	;; python-shell-interpreter-args "-i C:\\Anaconda\\Scripts\\ipython-script.py"
 	)

(defun my-python-send-region (&optional beg end)
   (interactive)
   (let ((beg (cond (beg beg)
                    ((region-active-p)
                     (region-beginning))
                    (t (line-beginning-position))))
         (end (cond (end end)
                    ((region-active-p)
                     (copy-marker (region-end)))
                    (t (line-end-position)))))
     (python-send-region beg end)))

;; ;;;; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
;; ;;;; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p t)

;; ;;;; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ;;;; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; ;;;; try to automagically figure out indentation
;; (setq py-smart-indentation t)

(require 'ein)
