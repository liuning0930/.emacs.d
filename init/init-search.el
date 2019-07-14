(require-package 'multiple-cursors)
(require 'multiple-cursors)

(require-package 'evil-mc)
(require 'evil-mc)

(setq slegetank/evil-mc-pinyin-need-restore nil)

(defun slegetank/evil-mc-enable ()
  (interactive)
  "Enable multiple-cursors mode in evil"
  (evil-mc-mode 1)
  (evil-mc-make-all-cursors)
  (message "Enter multi cursors mode."))

(defun slegetank/evil-mc-quit ()
  (interactive)
  "Disaable multiple-cursors mode in evil"
  (evil-mc-mode -1)
  (evil-mc-undo-all-cursors)
  (when slegetank/evil-mc-pinyin-need-restore
    (evil-find-char-pinyin-mode +1))
  (message "Quit multi cursors mode."))

(evil-define-key 'normal evil-mc-key-map (kbd "C-g") 'slegetank/evil-mc-quit)
(evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'slegetank/evil-mc-quit)

(add-hook 'evil-mc-mode-hook (lambda ()
                              (when (bound-and-true-p evil-find-char-pinyin-mode)
                                (evil-find-char-pinyin-mode -1)
                                (setq slegetank/evil-mc-pinyin-need-restore t))))

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(require-package 'avy)
(setq avy-case-fold-search nil)

(require-package 'ace-jump-mode)
(setq ace-jump-mode-case-fold nil)

(require-package 'anzu)
(add-hook 'after-init-hook 'global-anzu-mode)
(setq anzu-mode-lighter "")
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

(require-package 'evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu))

(require-package 'wgrep)
(require 'wgrep)

;; 编辑完自动保存
(setq wgrep-auto-save-buffer t)

(defun slegetank/wgrep-mode-setup ()
  "Custom wgrep mode setup."
  (interactive)
  (with-current-buffer (current-buffer)
    (setq-local
     header-line-format
     (substitute-command-keys
      " [C-c C-c]-commit, [C-c C-k]-cancel, [q]-quit")))
  (ivy-wgrep-change-to-wgrep-mode))

(add-hook 'ivy-occur-grep-mode-hook (lambda ()
                                      (evil-define-key 'normal ivy-occur-grep-mode-map (kbd "e") 'slegetank/wgrep-mode-setup)
                                      (evil-define-key 'normal ivy-occur-grep-mode-map (kbd "x") 'winnow-exclude-lines)
                                      (evil-define-key 'normal ivy-occur-grep-mode-map (kbd "m") 'winnow-match-lines)
                                      (evil-define-key 'normal wgrep-mode-map (kbd "q") (lambda ()
                                                                                          (interactive)
                                                                                          (kill-buffer)))) t)

(require-package 'winnow)
(require 'winnow)
;; (add-hook 'compilation-mode-hook 'winnow-mode)
;; (add-hook 'ag-mode-hook 'winnow-mode)

;; help
(define-key 'help-command (kbd "v") 'counsel-describe-variable)
(define-key 'help-command (kbd "f") 'counsel-describe-function)

(define-key evil-normal-state-map (kbd "s-f") 'swiper)
(define-key global-map (kbd "s-f") 'swiper)

(evil-define-key 'normal 'global (kbd "s-m") 'slegetank/evil-mc-enable)
(evil-define-key 'normal 'global (kbd "s-g") 'avy-goto-char)
