(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(setq no-trailing-modes '(minibuffer-setup-hook
                          eww-mode-hook
                          ielm-mode-hook
                          gdb-mode-hook
                          help-mode-hook
                          artist-mode-hook
                          term-mode-hook
                          mu4e-view-mode-hook
                          mu4e-org-mode-hook
                          mu4e-main-mode-hook
                          eshell-mode-hook))
(dolist (element no-trailing-modes nil)
  (add-hook element 'no-trailing-whitespace))

(require-package 'smart-hungry-delete)
(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(evil-global-set-key 'normal (kbd "C-d") 'smart-hungry-delete-forward-char)
