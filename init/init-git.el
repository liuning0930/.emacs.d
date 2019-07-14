(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(require-package 'magit)
(require-package 'evil-magit)
(require 'evil-magit)

;; large file slow
(setq magit-commit-show-diff nil)

(setq-default magit-diff-refine-hunk t)

(slegetank/leader-define-key "g" nil "Git"
                             "gs" 'magit-status "Git status"
                             "gm" 'magit-dispatch-popup "Menu for Magit")

;; url可点击
(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; (require-package 'git-gutter)
;; (global-git-gutter-mode +1)

(require-package 'git-messenger)
(setq git-messenger:show-detail t)
(slegetank/leader-define-key "gl" 'git-messenger:popup-message "Show last git commit of this line.")

(add-hook 'find-file-hook (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1))))
          t)

(defun slegetank/smerge-mode-setup ()
  (slegetank/leader-define-key
                               "gn" 'smerge-next "Next conflict"
                               "gp" 'smerge-prev "Prev conflict")
  (run-with-timer 0.1 nil 'smerge-next))
(add-hook 'smerge-mode-hook 'slegetank/smerge-mode-setup)

(defun slegetank/git-xcode-project ()
  "Open current XCode project path with magit"
  (interactive)
  (dired (file-name-as-directory (file-name-directory (s-trim (shell-command-to-string "osascript -e 'tell application id \"com.apple.dt.Xcode\" to return path of document 1'")))))
  (magit-status))

(defun slegetank/git-android-project ()
  "Open current XCode project path with magit"
  (interactive)
  (dired (file-name-as-directory (file-name-directory (s-chop-suffix "]"
                                                                     (s-chop-prefix "["
                                                                                    (car (s-match "\\[.*?\\]"
                                                                                                  (s-trim
                                                                                                   (shell-command-to-string "osascript -e 'tell application \"System Events\" to get the {title} of window 1 of process \"Android Studio\"'")))))))))
  (magit-status))

(slegetank/leader-define-key "gx" 'slegetank/git-xcode-project "Git xcode"
                             "ga" 'slegetank/git-android-project "Git android")
