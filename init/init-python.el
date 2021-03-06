(setq python-indent-offset 4)

(require-package 'elpy)
(setq eldoc-idle-delay 1)

(defun my-elpy-mode ()
  (make-variable-buffer-local 'elpy-modules)
  (setq elpy-modules
        '(elpy-module-sane-defaults
          elpy-module-company
          elpy-module-eldoc
          elpy-module-flymake
          ;; This eat too much CPU cycle
          ;; elpy-module-highlight-indentation
          elpy-module-pyvenv
          elpy-module-yasnippet))
  (setq elpy-eldoc-show-current-function nil)
  (evil-define-key 'normal elpy-mode-map (kbd "s-.") 'elpy-goto-definition)
  (elpy-mode))

(elpy-enable)
(add-hook 'python-mode-hook 'my-elpy-mode)

;; (require-package 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; (defun anaconda-key-bind-method ()
;;   "Custom key binding"
;;   (evil-define-key 'normal anaconda-mode-map (kbd "s-.") 'anaconda-mode-find-definitions)
;;   (evil-define-key 'normal anaconda-mode-map (kbd "s-?") 'anaconda-mode-show-doc))

;; (add-hook 'anaconda-mode-hook 'anaconda-key-bind-method)

;; (require-package 'company-anaconda)
;; (eval-after-load "company"
;;   '(add-to-list 'company-backends 'company-anaconda))
;; (add-hook 'python-mode-hook 'anaconda-mode)

(require-package 'flycheck-pyflakes)
(require 'flycheck-pyflakes)

(require-package 'importmagic)
(add-hook 'python-mode-hook (lambda ()
                              (importmagic-mode)
                              (slegetank/leader-define-key "pyi" 'importmagic-fix-symbol-at-point "Fix import issues")))

;; (require-package 'py-yapf)
;; (require 'py-yapf)
;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

(require-package 'sphinx-doc)
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)
                              (slegetank/leader-define-key "pyd" 'sphinx-doc "Doc for method")))

(setq slegetank/python-breakpoint nil)

(defun slegetank/python-decide-breakpoint ()
  (unless slegetank/python-breakpoint
    (if (= (shell-command "python3 -c \"import ipdb\"") 0)
        (setq slegetank/python-breakpoint "import ipdb; ipdb.set_trace();")
      (setq slegetank/python-breakpoint "import pdb; pdb.set_trace();"))))

(defun slegetank/add-py-breakpoint ()
  "Add python breakpoint."
  (interactive)
  (when (boundp 'evil-mode)
    (evil-open-below 1)
    (insert slegetank/python-breakpoint)
    (evil-normal-state)))

(defun slegetank/python-config ()
  "Config for python."
  (local-set-key (kbd "s-\\") 'slegetank/add-py-breakpoint)
  (slegetank/python-decide-breakpoint)
  (highlight-lines-matching-regexp (format "^[ ]*%s" slegetank/python-breakpoint))
  (local-set-key (kbd "<backspace>") 'python-indent-dedent-line-backspace))

(add-hook 'python-mode-hook 'slegetank/python-config)
