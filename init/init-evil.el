;; (setq evil-want-integration nil)
(require-package 'evil)

(require-package 'which-key)
(which-key-mode 1)
(setq which-key-side-window-max-height 0.25)
(setq which-key-idle-delay 0.3)
;; (which-key-add-key-based-replacements
;;   "<SPC>f" "File"
;;   "<SPC>d" "Directory"
;;   "<SPC>b" "Buffer"
;;   "<SPC>bl" "Blog"
;;   )

(require-package 'evil-leader)
(require 'evil-leader)

(setq evil-leader/in-all-states t)
(setq evil-leader/leader "SPC")
(global-evil-leader-mode)
(evil-mode t)
(setq evil-leader/no-prefix-mode-rx '("w3m.*-mode" "cfw:calendar-mode" "bookmark-bmenu-mode")) ; w3m mode needs this too!

(defun slegetank/leader-define-key (key def desc &rest seq)
  "Custom method to define key and add comment to which-key."
  (let ((map evil-leader--default-map)
        (fkey (concat "<SPC>" key))
        (fdesc desc)
        keyDescList)
    (while key
      (when def
        (define-key map key def))
      (setq key (pop seq)
            def (pop seq)
            desc (pop seq))
      (when key
        (push desc keyDescList)
        (push (concat "<SPC>" key) keyDescList)))
    (apply 'which-key-add-key-based-replacements fkey fdesc keyDescList)))

(defun slegetank/repeat-last-command ()
  "Custom repeat last command."
  (interactive)
  (let ((lastcommand nil)
        (tempcommand nil))
    (when (/= 0 (length command-history))
      (setq tempcommand command-history)
      (while (and (not lastcommand) tempcommand)
        (if (where-is-internal (caar tempcommand) evil-leader--default-map)
            (setq lastcommand (caar tempcommand))
          (setq tempcommand (cdr tempcommand)))))
    (when lastcommand
      (call-interactively lastcommand))))

(slegetank/leader-define-key "f" nil "Files"
                             "ff" 'counsel-find-file "Find file"
                             "fl" 'find-file-literally "Find large file"
                             "b" nil "Buffers"
                             "bb" 'ivy-switch-buffer "Switch buffer"
                             "bk" 'kill-buffer "Kill buffer"
                             "bm" nil "Bookmark"
                             "bo" 'ivy-switch-buffer-other-window "Switch buffer in other window"
                             "bB" 'ivy-switch-buffer-other-window "Switch buffer in other window"
                             "bc" 'slegetank/clear-message-buffer "Clear message buffer"
                             "bmm" 'bookmark-set "Bookmark set"
                             "bml" 'bookmark-bmenu-list "Bookmark list"
                             "bmj" 'counsel-bookmark "Bookmark jump"
                             "e" nil "elisp"
                             "er" 'eval-region "Eval region"
                             "eb" 'eval-buffer "Eval buffer"
                             "." 'slegetank/repeat-last-command "Repeat")

;; q for kill-buffer, not for exit emacss
(evil-ex-define-cmd "q" (lambda () (interactive) (kill-buffer (current-buffer))))
;; wq for save & kill-buffer, not for exit emacss
(evil-ex-define-cmd "wq" (lambda () (interactive) (save-buffer) (kill-buffer (current-buffer))))

(evil-define-key '(normal insert visual replace operator motion) 'global (kbd "C-z") 'undo-tree-undo)

;; (require-package 'evil-collection)
;; (require 'evil-collection)
;; ;; (evil-collection-init)

;; (evil-collection-init 'notmuch)

(require-package 'evil-nerd-commenter)

(evilnc-default-hotkeys)
(global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)

(require-package 'evil-surround)

(global-evil-surround-mode 1)

(require-package 'evil-matchit)
(global-evil-matchit-mode 1)
(add-hook 'dired-mode-hook (lambda ()
                             "dired不需要这个功能"
                             (turn-off-evil-matchit-mode)))

(require-package 'evil-visualstar)
(global-evil-visualstar-mode)

(require-package 'evil-cleverparens)

(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(setq evil-move-beyond-eol t)

(define-key evil-visual-state-map [escape] 'evil-visual-char)

(require-package 'expand-region)
(require 'expand-region)

(defun evil-visual-char-or-expand-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'er/expand-region)
    (evil-visual-char)))

(define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)

;; (defun slegetank/evil-select-pasted ()
;;   "Visually select last pasted text."
;;   (interactive)
;;   (evil-goto-mark ?[)
;;                   (evil-visual-char)
;;                   (evil-goto-mark ?]))

;; (slegetank/leader-define-key "gp" 'slegetank/evil-select-pasted "Select last paste word")

;; (when (and (equal system-type 'darwin)
;;            (executable-find "keyboardSwitcher"))
;;   (let ((temp-text (shell-command-to-string "keyboardSwitcher enabled")))
;;     (if (s-contains? "ABC" temp-text)
;;         (setq slegetank/en-input-source "ABC")
;;       (setq slegetank/en-input-source "U.S.")))

;;   (add-hook 'evil-normal-state-entry-hook
;;             (lambda ()
;;               (call-process-shell-command (format "keyboardSwitcher select %s" slegetank/en-input-source) nil 0)))

;;   (add-hook 'focus-in-hook (lambda ()
;;                              "When focus on emacs, if state is normal, change input method to english"
;;                              (when (equal evil-state 'normal)
;;                                (call-process-shell-command (format "keyboardSwitcher select %s" slegetank/en-input-source) nil 0)))))
